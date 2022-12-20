;;;
;;; Test makiki
;;;

(use file.util)
(use gauche.net)
(use gauche.parameter)
(use gauche.process)
(use gauche.test)
(use rfc.822)
(use rfc.http)
(use scheme.list)
(use srfi.98)

(test-start "makiki")
(use makiki)
(test-module 'makiki)

(define (call-with-server path proc)
  (let* ([p (run-process `(gosh "-I." ,path "--port" 0)
                            :output :pipe :error :pipe :wait #f)]
         [port (get-port p path)]
         [host #"localhost:~port"])
    (unwind-protect (proc p host port)
      (process-kill p))))

(define (call-with-server/exit-status path proc)
  (let* ([p (run-process `(gosh "-I." ,path "--port" 0)
                         :output :pipe :error :pipe :wait #f)]
         [port (get-port p path)]
         [host #"localhost:~port"])
    (proc p host port)
    (process-wait p)
    (sys-wait-exit-status (process-exit-status p))))

(define (get-port proc path)
  (rxmatch-case (read-line (process-error proc))
    [#/started on \(.*:(\d+)\)/i (_ pp) (x->integer pp)]
    [else => (cut errorf "failed to start server script ~s: ~a" path <>)]))

;;;
(test-section "basic functionality")

($ call-with-server "tests/basic.scm"
   (^[p s t]
     (test* "basic responds 404" "404"
            (values-ref (http-get s "/") 0))

     (test* "bad request format" (eof-object)
            (let1 s (make-client-socket 'inet "localhost" t)
              (socket-shutdown s SHUT_WR)
              (unwind-protect (read-line (socket-input-port s))
                (socket-close s))))

     (test* "bad request format" "HTTP/1.1 501 Not Implemented"
            (let1 s (make-client-socket 'inet "localhost" t)
              (unwind-protect
                  (begin
                    (display "ZZZZ /foo.txt HTTP/1.1\r\n" (socket-output-port s))
                    (socket-shutdown s SHUT_WR)
                    (read-line (socket-input-port s)))
                (socket-close s))))
     ))

;;;
(test-section "request methods")

($ call-with-server "tests/methods.scm"
   (^[p s t]
     (test* "GET"    "get"    (values-ref (http-get s "/") 2))
     (test* "POST"   "post"   (values-ref (http-post s "/" "") 2))
     (test* "PUT"    "put"    (values-ref (http-put s "/" "") 2))
     (test* "DELETE" "delete" (values-ref (http-delete s "/") 2))

     (test* "OPTIONS" "HTTP/1.1 404 Not Found"
            (let1 s (make-client-socket 'inet "localhost" t)
              (unwind-protect
                  (begin
                    (display "OPTIONS / HTTP/1.1\r\n" (socket-output-port s))
                    (socket-shutdown s SHUT_WR)
                    (read-line (socket-input-port s)))
                (socket-close s))))
     ))

;;;
(test-section "file-handler")

($ call-with-server "tests/file.scm"
   (^[p s t]
     (define (file-test path ctype)
       (receive (code hdrs body) (http-get s #"/~path")
         (test* (format "file test ~s" path) `("200" ,ctype #t)
                (list code (rfc822-header-ref hdrs "content-type")
                      (equal? (file->string path) body)))))
     (file-test "tests/file.scm" "text/plain")
     (file-test "tests/file-dummy.js" "application/javascript; charset=utf-8")
     (file-test "tests/file-dummy.gif" "image/gif")
     (file-test "tests/file-dummy.jpg" "image/jpeg")
     (file-test "tests/file-dummy.png" "image/png")
     (file-test "tests/file-dummy.css" "text/css")
     ))

;;;
(test-section "cgi-handler and cgi-script")
(use makiki.cgi)
(test-module 'makiki.cgi)

(define (test-cgi-stuff server-file)
  ($ call-with-server server-file
     (^[p s t]
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

(test-cgi-stuff "tests/test-env1.scm")
(test-cgi-stuff "tests/test-env2.scm")

;;;
(test-section "customized respond/ng body")

($ call-with-server "tests/customized-ng.scm"
   (^[p s t]
     (test* "custom 404 response body"
            '("404" "text/html; charset=utf-8"
              "<html><title>404 Not found</title><body><p>I don't have that!</p></body></html>")
            (receive (code hdrs body) (http-get s "/")
              (list code (rfc822-header-ref hdrs "content-type") body)))))

($ call-with-server "tests/customized-ng.scm"
   (^[p s t]
     (test* "error in custom 404 response body"
            '("404" "text/plain; charset=utf-8"
              "Not Found")
            (receive (code hdrs body) (http-get s "/favicon.ico")
              (list code (rfc822-header-ref hdrs "content-type") body)))))

($ call-with-server "tests/customized-ng.scm"
   (^[p s t]
     (test* "request-error condition"
            '("400" "application/json; charset=utf-8"
              "{\"message\":\"boo!\"}")
            (receive (code hdrs body) (http-get s "/request-error")
              (list code (rfc822-header-ref hdrs "content-type") body)))))

;;;
(test-section "sxml template")

($ call-with-server "tests/sxml-tmpl.scm"
   (^[p s t]
     (test* "sxml-tmpl"
            "<html><body><p>Yo, Keoki.  Howzit?</p></body></html>"
            (values-ref (http-get s "/?g=2&name=Keoki") 2))))

;;;
(test-section "json")

($ call-with-server "tests/json-server.scm"
   (^[p s t]
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
(test-section "let-params")

(use rfc.cookie)

($ call-with-server "tests/let-params.scm"
   (^[p s t]
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

($ call-with-server "tests/server-error.scm"
   (^[p s t]
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
       ($ call-with-server/exit-status "tests/termination.scm"
          (^[p s t] (http-get s "/a"))))

(test* "/b" 2
       ($ call-with-server/exit-status "tests/termination.scm"
          (^[p s t] (http-get s "/b"))))

;;;
(test-section "profiling")

(define *profiler.out* "profiler.out")

(unwind-protect
    (^[]
      (test* "profile output" #t
             (begin
               (sys-unlink *profiler.out*)
               ($ call-with-server "tests/profiler.scm"
                  (^[p s t] (http-get s "/profile")))
               (and (file-exists? *profiler.out*)
                    (> (file-size *profiler.out*) 0))))
      (test* "profile output" #f
             (begin
               (sys-unlink *profiler.out*)
               ($ call-with-server "tests/profiler.scm"
                  (^[p s t] (http-get s "/noprofile")))
               (and (file-exists? *profiler.out*)
                    (> (file-size *profiler.out*) 0)))))
  (sys-unlink *profiler.out*))

;;;
(test-section "add-on modules")

(use makiki.connect)
(test-module 'makiki.connect)

(use makiki.cgi)
(test-module 'makiki.cgi)

;; epilogue
(test-end :exit-on-failure #t)
