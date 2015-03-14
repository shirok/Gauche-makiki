;;;
;;; Test makiki
;;;

(use gauche.test)
(use gauche.process)
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

(test-section "cgi-handler and cgi-script")

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

(test-section "sxml template")

($ call-with-server "tests/sxml-tmpl.scm"
   (^p
    (test* "sxml-tmpl"
           "<html><body><p>Yo, Keoki.  Howzit?</p></body></html>"
           (values-ref (http-get *server* "/?g=2&name=Keoki") 2))))

(test-section "add-on modules")

(use makiki.connect)
(test-module 'makiki.connect)

;; epilogue
(test-end)
