(use gauche.parseopt)
(use makiki)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p)))

(define-http-handler "/a"
  (^[req app] (terminate-server-loop req 1)))

(define-http-handler "/b"
  (^[req app] (terminate-server-loop req 2)))
