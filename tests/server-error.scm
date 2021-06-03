(use makiki)
(use gauche.parameter)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log "test-server.log" :port p))
  0)

(define-http-handler "/"
  (^[req app]
    (let ([x (request-param-ref req "foo")]
          [debug (request-param-ref req "debug")])
      (debugging (equal? debug "true"))
      (raise-an-error x)
      "make-it-no-tail-call")))

(define (raise-an-error x)
  (sqrt x)                      ;this always raise an error
  "make-it-no-tail-call")
