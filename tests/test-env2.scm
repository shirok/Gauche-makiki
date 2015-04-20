(use makiki)
(use makiki.cgi)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t ;; required to make handshake work
                       :error-log "test-error-env2.log"
                       :port p))
  0)

(define-http-handler "/"
  (cgi-script "tests/env.scm"))


