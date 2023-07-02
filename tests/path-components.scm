(use gauche.parseopt)
(use makiki)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define-http-handler ("foo" "bar")
  (^[req app] (respond/ok req "foo bar")))
(define-http-handler ("foo" var)
  (^[req app]
    (respond/ok req #"foo var=~((request-path-rxmatch req) 'var)")))
(define-http-handler ("foo" var "bar")
  (^[req app]
    (respond/ok req #"foo var=~((request-path-rxmatch req) 'var) bar")))

(define-http-handler (POST) ("boo" x)
  (^[req app] (respond/ok req #"POST:boo x=~((request-path-rxmatch req) 'x)")))
(define-http-handler (GET) ("boo" x)
  (^[req app] (respond/ok req #"GET:boo x=~((request-path-rxmatch req) 'x)")))
