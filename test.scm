;;;
;;; Test makiki
;;;

(use file.util)
(use gauche.net)
(use gauche.process)
(use gauche.test)
(use gauche.version)
(use rfc.822)
(use rfc.http)
(use rfc.tls)
(use scheme.list)
(use srfi.98)

(test-start "makiki")
(use makiki)
(test-module 'makiki
             ;; these symbols are only available in Gauche 0.9.14 or later
             :allow-undefined '(tls-poll
                                tls-bind
                                tls-load-private-key
                                tls-load-certificate))
(use makiki.subserver)
(test-module 'makiki.subserver)

;; TRANSIENT: After Gauche 1.0, we can use (build-path 'cld "tests" path)
(define (testdir path)
  (build-path (sys-dirname (current-load-path)) "tests" path))


;;;
(test-section "basic functionality")

($ call-with-httpd (testdir "basic.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "basic responds 404" "404"
            (values-ref (http-get s "/") 0))

     (test* "bad request format" (eof-object)
            (let1 s (make-client-socket 'inet "localhost" port)
              (socket-shutdown s SHUT_WR)
              (unwind-protect (read-line (socket-input-port s))
                (socket-close s))))

     (test* "bad request format" "HTTP/1.1 501 Not Implemented"
            (let1 s (make-client-socket 'inet "localhost" port)
              (unwind-protect
                  (begin
                    (display "ZZZZ /foo.txt HTTP/1.1\r\n" (socket-output-port s))
                    (socket-shutdown s SHUT_WR)
                    (read-line (socket-input-port s)))
                (socket-close s))))
     ))

;;;
(test-section "request methods")

($ call-with-httpd (testdir "methods.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "GET"    "get"    (values-ref (http-get s "/") 2))
     (test* "POST"   "post"   (values-ref (http-post s "/" "") 2))
     (test* "PUT"    "put"    (values-ref (http-put s "/" "") 2))
     (test* "DELETE" "delete" (values-ref (http-delete s "/") 2))

     (test* "OPTIONS" "HTTP/1.1 404 Not Found"
            (let1 s (make-client-socket 'inet "localhost" port)
              (unwind-protect
                  (begin
                    (display "OPTIONS / HTTP/1.1\r\n" (socket-output-port s))
                    (socket-shutdown s SHUT_WR)
                    (read-line (socket-input-port s)))
                (socket-close s))))
     ))

;;;
(test-section "component match")
($ call-with-httpd (testdir "path-components.scm")
   (^[port]
     (define s #"localhost:~port")
     (define (g path expect)
       (test* path expect (values-ref (http-get s path) 2)))
     (define (p path expect)
       (test* path expect (values-ref (http-post s path "") 2)))
     (g "/foo/bar" "foo bar")
     (g "/foo/baz" "foo var=baz")
     (g "/foo/baz/bar" "foo var=baz bar")
     (g "/foo/bar/" "foo bar")

     (g "/boo/bar" "GET:boo x=bar")
     (p "/boo/bar" "POST:boo x=bar")

     (g "/hoo/1234-5678" "hoo x=1234-5678 y=5678")
     (g "/hoo/123456789" "hoo z=123456789")
     (g "/hoo/var"       "hoo z=var")

     (g "/int/483753" "int n=483753")
     (g "/int/53cafe" "int x=53cafe")
     (g "/hex/53cafe" "hex n=5491454")
     (g "/hex/53cafeg" "hex x=53cafeg")

     (g "/sym/abcde" "sym y=abcde")
     (g "/sym/foo_123" "sym y=foo_123")
     (g "/sym/foo-123" "sym #f")

     (cond-expand
      [(library rfc.uuid)
       (g "/uuid/cf33321e-18b4-46cf-b8a6-d6b09e743d00"
          "uuid u=cf33321e-18b4-46cf-b8a6-d6b09e743d00")
       (g "/uuid/cf33321e"
          "uuid #f")]
      [else])
     ))

;;;
(test-section "file-handler")

($ call-with-httpd (testdir "file.scm")
   (^[port]
     (define s #"localhost:~port")
     (define (file-test path ctype)
       (receive (code hdrs body) (http-get s #"/~path")
         (test* (format "file test ~s" path) `("200" ,ctype #t)
                (list code (rfc822-header-ref hdrs "content-type")
                      (equal? (file->string (testdir path)) body)))))
     (file-test "file.scm" "text/plain")
     (file-test "file-dummy.js" "application/javascript; charset=utf-8")
     (file-test "file-dummy.gif" "image/gif")
     (file-test "file-dummy.jpg" "image/jpeg")
     (file-test "file-dummy.png" "image/png")
     (file-test "file-dummy.css" "text/css")
     ))

;;;
(test-section "cgi-handler and cgi-script")
(use makiki.cgi)
(test-module 'makiki.cgi)

(define (test-cgi-stuff server-file)
  ($ call-with-httpd server-file
     (^[port]
       (define s #"localhost:~port")
       (test* #"(~server-file) empty parameters" (get-environment-variables)
              (assq-ref (read-from-string (values-ref (http-get s "/") 2))
                        'environments)
              (cut lset= equal? <> <>))
       (test* #"(~server-file) with parameters"
              '(("a" ")(!#%%$!*^({}<>" "wat?") ("b" ""))
              (assq-ref (read-from-string
                         (values-ref (http-get s '("/"
                                                   (a ")(!#%%$!*^({}<>")
                                                   (b "")
                                                   (a "wat?")))
                                     2))
                        'parameters)
              (cut lset= equal? <> <>))
       )))

(test-cgi-stuff (testdir "test-env1.scm"))
(test-cgi-stuff (testdir "test-env2.scm"))

;;;
(test-section "customized respond/ng body")

($ call-with-httpd (testdir "customized-ng.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "custom 404 response body"
            '("404" "text/html; charset=utf-8"
              "<!DOCTYPE html>\n<html><title>404 Not found</title><body><p>I don't have that!</p></body></html>")
            (receive (code hdrs body) (http-get s "/")
              (list code (rfc822-header-ref hdrs "content-type") body)))))

($ call-with-httpd (testdir "customized-ng.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "error in custom 404 response body"
            '("404" "text/plain; charset=utf-8"
              "Not Found")
            (receive (code hdrs body) (http-get s "/favicon.ico")
              (list code (rfc822-header-ref hdrs "content-type") body)))))

($ call-with-httpd (testdir "customized-ng.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "request-error condition"
            '("400" "application/json; charset=utf-8"
              "{\"message\":\"boo!\"}")
            (receive (code hdrs body) (http-get s "/request-error")
              (list code (rfc822-header-ref hdrs "content-type") body)))))

;;;
(test-section "sxml template")

($ call-with-httpd (testdir "sxml-tmpl.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "sxml-tmpl"
            "<!DOCTYPE html>\n<html><body><p>Yo, Keoki.  Howzit?</p></body></html>"
            (values-ref (http-get s "/?g=2&name=Keoki") 2))))

;;;
(test-section "json")

($ call-with-httpd (testdir "json-server.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "json request/response" "{\"count\":0}"
            (values-ref (http-get s "/") 2))
     (test* "json request/response" "{\"count\":101}"
            (values-ref (http-post s "/" "{\"count\":100}") 2))
     (test* "json request/response (bad body)"
            '("400" #/^invalid json/)
            (receive (code hdrs body)
                (http-post s "/" "[bad")
              (list code body))
            (^[a b] (and (equal? (car a) (car b))
                         ((cadr a) (cadr b)))))))

;;;
(test-section "set-cookie")

($ call-with-httpd (testdir "cookie.scm")
   (^[port]
     (define s #"localhost:~port")
     (define (t query expects . opts)
       (test* "set-cookie" expects
              (receive [c hdrs body] (apply http-get s #"/?~query" opts)
                (pprint hdrs)
                (filter-map (^[hdr]
                              (and (equal? (car hdr) "set-cookie")
                                   (cadr hdr)))
                            hdrs))
              (cut lset= equal? <> <>)))
     (t "cookie-test=foo" '("cookie-test=foo;Max-Age=60"))
     (t "a=b&c=d" '("a=b;Max-Age=60" "c=d;Max-Age=60"))

     (t "foo=bar" '("foo=bar;Secure;Max-Age=60")
        :x-forwarded-proto "https")
     ))

;;;
(test-section "let-params")

(use rfc.cookie)

($ call-with-httpd (testdir "let-params.scm")
   (^[port]
     (define s #"localhost:~port")
     (define (req path . args)
       (read-from-string (values-ref (apply http-get s path args) 2)))
     (parameterize ([http-user-agent "makiki-test"])
       (test* "let-params 1"
              '("moo" doo "zoo" 123456 "makiki-test" #f #f -1)
              (req "/zoo/foo-123456?param1=moo&poo=doo"))
       (test* "let-params 2"
              '(none #t "zoo" 654321 "makiki-test" (a b c) "coo" 4934)
              (req "/zoo/foo-654321"
                   :x-header2 "(a b c)"
                   :cookie "cookie1=coo;cookie2=4934"))
       )))

;;;
(test-section "server error handler")

($ call-with-httpd (testdir "server-error.scm")
   (^[port]
     (define s #"localhost:~port")
     (test* "server error handler" '("500" "Internal Server Error")
            (receive (status hdrs body) (http-get s "/?foo=z")
              (list status body)))
     (test* "server error handler" '("500" "Internal Server Error")
            (receive (status hdrs body)
                (http-get s "/?foo=z" :accept "text/plain")
              (list status body)))
     (test* "server error handler" '("500" "<html><head><title>Internal Server Error</title\n></head\n><body><h1>Internal Server Error</h1\n><p></p\n></body\n></html\n>")
            (receive (status hdrs body)
                (http-get s "/?foo=z" :accept "text/html")
              (list status body)))
     (test* "server error handler" '("500" "{\"status\":500,\"message\":\"Internal Server Error\"}")
            (receive (status hdrs body)
                (http-get s "/?foo=z" :accept "application/json")
              (list status body)))

     (test* "server error handler" '("500" "Internal Server Error: number required, but got \"z\"")
            (receive (status hdrs body)
                (http-get s "/?foo=z&debug=true" :accept "text/plain")
              (list status body)))
     (test* "server error handler" '("500" "<html><head><title>Internal Server Error</title\n></head\n><body><h1>Internal Server Error</h1\n><p>number required, but got \"z\"</p\n></body\n></html\n>")
            (receive (status hdrs body)
                (http-get s "/?foo=z&debug=true" :accept "text/html")
              (list status body)))
     (test* "server error handler" '("500" "{\"status\":500,\"message\":\"Internal Server Error: number required, but got \\\"z\\\"\"}")
            (receive (status hdrs body)
                (http-get s "/?foo=z&debug=true" :accept "application/json")
              (list status body)))
     ))

;;;
(test-section "server termination")

(test* "/a" 1
       ($ call-with-httpd/wait (testdir "termination.scm")
          (^[port] (http-get #"localhost:~port" "/a"))))

(test* "/b" 2
       ($ call-with-httpd/wait (testdir "termination.scm")
          (^[port] (http-get #"localhost:~port" "/b"))))

;;;
(test-section "profiling")

(define *profiler.out* "profiler.out")

(unwind-protect
    (^[]
      (test* "profile output" #t
             (begin
               (sys-unlink *profiler.out*)
               ($ call-with-httpd (testdir "profiler.scm")
                  (^[port] (http-get #"localhost:~port" "/profile")))
               (and (file-exists? *profiler.out*)
                    (> (file-size *profiler.out*) 0))))
      (test* "profile output" #f
             (begin
               (sys-unlink *profiler.out*)
               ($ call-with-httpd (testdir "profiler.scm")
                  (^[port] (http-get #"localhost:~port" "/noprofile")))
               (and (file-exists? *profiler.out*)
                    (> (file-size *profiler.out*) 0)))))
  (sys-unlink *profiler.out*))

;;;
(test-section "add-on modules")

(use makiki.connect)
(test-module 'makiki.connect)

(use makiki.dev)
(test-module 'makiki.dev)

(use makiki.session)
(test-module 'makiki.session)

;;;
(test-section "sessions")

($ call-with-httpd (testdir "session.scm")
   (^[port]
     (define s #"localhost:~port")
     (define session-key #f)
     (define (get-session-key hdrs)
       (and-let* ([v (rfc822-header-ref hdrs "set-cookie")]
                  [m (#/^makiki-session=(.*)/ v)])
         (m 1)))
     (test* "query before login" "none"
            (values-ref (http-get s "/query") 2))
     (test* "login" "OK"
            (receive (code hdrs body) (http-get s "/login")
              (set! session-key (get-session-key hdrs))
              (and (string? session-key) body)))
     (test* "query after login" "bar"
            (values-ref (http-get s "/query"
                                  :cookie #"makiki-session=~|session-key|")
                        2))
     (test* "guard with session" "Yo"
            (values-ref (http-get s "/anything"
                                  :cookie #"makiki-session=~|session-key|")
                        2))
     (test* "logout" ";Expires=Thu, 01-Jan-1970 00:00:00 GMT"
            (receive (code hdrs body)
                (http-get s "/logout"
                          :cookie #"makiki-session=~|session-key|")
              (get-session-key hdrs)))
     (test* "query after logout" "none"
            (values-ref (http-get s "/query") 2))
     (test* "guard with session (logout)" "Huh?"
            (values-ref (http-get s "/anything") 2))
     ))

;;;
(when (version>=? (gauche-version) "0.9.14")
  (test-section "https")
  ($ call-with-httpd (testdir "tls.scm")
     (^[port]
       (parameterize ((tls-ca-bundle-path (testdir "data/test-cert.pem")))
         (test* "TLS connection" "https ok"
                (values-ref (http-get #"localhost:~|port|" "/" :secure #t)
                            2)))))

  )

;; epilogue
(test-end :exit-on-failure #t)
