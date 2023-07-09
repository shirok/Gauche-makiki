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
    (respond/ok req #"foo var=~(request-path-ref req 'var)")))
(define-http-handler ("foo" var "bar")
  (^[req app]
    (respond/ok req #"foo var=~(request-path-ref req 'var) bar")))

(define-http-handler (POST) ("boo" x)
  (^[req app] (respond/ok req #"POST:boo x=~(request-path-ref req 'x)")))
(define-http-handler (GET) ("boo" x)
  (^[req app] (respond/ok req #"GET:boo x=~(request-path-ref req 'x)")))

(define-http-handler ("hoo" (#/^\d+-(\d+)$/ x y))
  (^[req app] (respond/ok req #"hoo x=~(request-path-ref req 'x) y=~(request-path-ref req 'y)")))
(define-http-handler ("hoo" z)
  (^[req app] (respond/ok req #"hoo z=~(request-path-ref req 'z)")))

(define-http-handler ("int" (path:int n))
  (^[req app] (respond/ok req #"int n=~(request-path-ref req 'n)")))
(define-http-handler ("int" x)
  (^[req app] (respond/ok req #"int x=~(request-path-ref req 'x)")))
(define-http-handler ("hex" (path:hex n))
  (^[req app] (respond/ok req #"hex n=~(request-path-ref req 'n)")))
(define-http-handler ("hex" x)
  (^[req app] (respond/ok req #"hex x=~(request-path-ref req 'x)")))

(define-http-handler ("sym" ((path:symbol #/^\w+$/) y))
  (^[req app] (respond/ok req #"sym y=~(request-path-ref req 'y)")))
(define-http-handler ("sym" _)
  (^[req app] (respond/ok req #"sym #f")))

(cond-expand
 [(library rfc.uuid)
  (use rfc.uuid)
  (define-http-handler ("uuid" (path:uuid u))
    (^[req app] (respond/ok req #"uuid u=~(uuid->string (request-path-ref req 'u))")))
  (define-http-handler ("uuid" _)
    (^[req app] (respond/ok req #"uuid #f")))]
 [else])
