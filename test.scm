;;;
;;; Test makiki
;;;

(use gauche.test)
(use gauche.process)
(use gauche.parameter)
(use gauche.net)
(use rfc.http)
(use rfc.822)
(use file.util)
(use srfi-1)
(use srfi-98)

(test-start "makiki")
(use makiki)
(test-module 'makiki)

(define *port* 8359)
(define *server* #`"localhost:,*port*")

(define (call-with-server path proc)
  (let ((p (run-process `(gosh "-I." ,path "--port" ,*port*)
                        :output :pipe :error :pipe :wait #f)))
    (let1 msg (read-line (process-error p))
      (unless (#/started/ msg)
        (errorf "failed to start server script ~s: ~a" path msg)))
    (unwind-protect (proc p)
      (process-kill p))))

(define (call-with-server/exit-status path proc)
  (let ((p (run-process `(gosh "-I." ,path "--port" ,*port*)
                        :output :pipe :error :pipe :wait #f)))
    (let1 msg (read-line (process-error p))
      (unless (#/started/ msg)
        (errorf "failed to start server script ~s: ~a" path msg)))
    (proc p)
    (process-wait p)
    (sys-wait-exit-status (process-exit-status p))))

;;;
(test-section "basic functionality")

($ call-with-server "tests/basic.scm"
   (^p
    (test* "basic responds 404" "404"
           (values-ref (http-get *server* "/") 0))

    (test* "bad request format" (eof-object)
           (let1 s (make-client-socket 'inet "localhost" *port*)
             (socket-shutdown s SHUT_WR)
             (unwind-protect (read-line (socket-input-port s))
               (socket-close s))))

    (test* "bad request format" "HTTP/1.1 501 Not Implemented"
           (let1 s (make-client-socket 'inet "localhost" *port*)
             (unwind-protect
                 (begin
                   (display "PUT /foo.txt HTTP/1.1\r\n" (socket-output-port s))
                   (socket-shutdown s SHUT_WR)
                   (read-line (socket-input-port s)))
               (socket-close s))))
    ))

;;;
(test-section "file-handler")

($ call-with-server "tests/file.scm"
   (^p
    (define (file-test path ctype)
      (receive (code hdrs body) (http-get *server* #`"/,path")
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
     (^p
      (test* #`"(,server-file) empty parameters" (get-environment-variables)
             (assq-ref (read-from-string (values-ref (http-get *server* "/") 2))
                       'environments)
             (cut lset= equal? <> <>))          
      (test* #`"(,server-file )with parameters"
             '(("a" ")(!#%%$!*^({}<>" "wat?") ("b" ""))
             (assq-ref (read-from-string
                        (values-ref (http-get *server* '("/"
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
   (^p
    (test* "custom 404 response body"
           '("404" "text/html; charset=utf-8"
             "<html><title>404 Not found</title><body><p>I don't have that!</p></body></html>")
           (receive (code hdrs body) (http-get *server* "/")
             (list code (rfc822-header-ref hdrs "content-type") body)))))

($ call-with-server "tests/customized-ng.scm"
   (^p
    (test* "error in custom 404 response body"
           '("404" "text/plain; charset=utf-8"
             "Not Found")
           (receive (code hdrs body) (http-get *server* "/favicon.ico")
             (list code (rfc822-header-ref hdrs "content-type") body)))))

($ call-with-server "tests/customized-ng.scm"
   (^p
    (test* "request-error condition"
           '("400" "application/json; charset=utf-8"
             "{\"message\":\"boo!\"}")
           (receive (code hdrs body) (http-get *server* "/request-error")
             (list code (rfc822-header-ref hdrs "content-type") body)))))

;;;
(test-section "sxml template")

($ call-with-server "tests/sxml-tmpl.scm"
   (^p
    (test* "sxml-tmpl"
           "<html><body><p>Yo, Keoki.  Howzit?</p></body></html>"
           (values-ref (http-get *server* "/?g=2&name=Keoki") 2))))

;;;
(test-section "json")

($ call-with-server "tests/json-server.scm"
   (^p
    (test* "json request/response" "{\"count\":0}"
           (values-ref (http-get *server* "/") 2))
    (test* "json request/response" "{\"count\":101}"
           (values-ref (http-post *server* "/" "{\"count\":100}") 2))))

;;;
(test-section "let-params")

(use rfc.cookie)

($ call-with-server "tests/let-params.scm"
   (^p
    (define (req path . args)
      (read-from-string (values-ref (apply http-get *server* path args) 2)))
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
(test-section "server termination")

(test* "/a" 1
       ($ call-with-server/exit-status "tests/termination.scm"
          (^_ (http-get *server* "/a"))))

(test* "/b" 2
       ($ call-with-server/exit-status "tests/termination.scm"
          (^_ (http-get *server* "/b"))))

;;;
(test-section "add-on modules")

(use makiki.connect)
(test-module 'makiki.connect)

(use makiki.cgi)
(test-module 'makiki.cgi)

;; epilogue
(test-end)
