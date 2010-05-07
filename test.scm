;;;
;;; Test makiki
;;;

(use gauche.test)
(use gauche.process)
(use rfc.http)

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
  (lambda (p)
    (test* "basic responds 404" "404"
           (values-ref (http-get *server* "/") 0))
    ))

;; epilogue
(test-end)





