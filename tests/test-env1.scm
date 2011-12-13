(use makiki)
(use gauche.parseopt)

(load "env.scm")
(define env-main main)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define-http-handler #/^\/$/
  (cgi-handler env-main :script-name "env.scm"))

