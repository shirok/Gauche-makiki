;;;
;;; Test makiki
;;;

(use gauche.test)
(use gauche.process)
(use gauche.net)
(use rfc.http)
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

(call-with-server "tests/basic.scm"
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

(test-section "cgi-handler")

(call-with-server "tests/test-env1.scm"
  (^p
   (test* "empty parameters" (get-environment-variables)
          (assq-ref (read-from-string (values-ref (http-get *server* "/") 2))
                    'environments)
          (cut lset= equal? <> <>))          
   (test* "with parameters" '(("a" ")(!#%%$!*^({}<>" "wat?") ("b" ""))
          (assq-ref (read-from-string
                     (values-ref (http-get *server* '("/"
                                                      (a ")(!#%%$!*^({}<>")
                                                      (b "")
                                                      (a "wat?")))
                                 2))
                    'parameters)
          (cut lset= equal? <> <>))
   ))

;; epilogue
(test-end)
