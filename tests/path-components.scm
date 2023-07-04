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
    (respond/ok req #"foo var=~((request-path-match req) 'var)")))
(define-http-handler ("foo" var "bar")
  (^[req app]
    (respond/ok req #"foo var=~((request-path-match req) 'var) bar")))

(define-http-handler (POST) ("boo" x)
  (^[req app] (respond/ok req #"POST:boo x=~((request-path-match req) 'x)")))
(define-http-handler (GET) ("boo" x)
  (^[req app] (respond/ok req #"GET:boo x=~((request-path-match req) 'x)")))

(define-http-handler ("hoo" (#/^\d+-(\d+)$/ x y))
  (^[req app] (respond/ok req #"hoo x=~((request-path-match req) 'x) y=~((request-path-match req) 'y)")))
(define-http-handler ("hoo" z)
  (^[req app] (respond/ok req #"hoo z=~((request-path-match req) 'z)")))
