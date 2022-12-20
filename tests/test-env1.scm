(use gauche.parseopt)
(use makiki)
(use makiki.cgi)

(load "tests/env.scm")
(define env-main main)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t ;; required to make handshake work
                       :error-log "test-error-env1.log"
                       :port p))
  0)

(define-http-handler "/"
  (cgi-handler env-main :script-name "env.scm"))
