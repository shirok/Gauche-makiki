(use gauche.parseopt)
(add-load-path ".." :relative)
(use makiki)
(use makiki.session)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define-http-handler "/login"
  (with-session
   (^[req app]
     (session-set! "bar")
     (respond/ok req "OK"))))

(define-http-handler "/query"
  (with-session
   (^[req app]
     (respond/ok req (session-ref "none")))))

(define-http-handler "/logout"
  (with-session
   (^[req app]
     (session-delete!)
     (respond/ok req "OK"))))

(define-http-handler #/.*/
  ? (guard-with-session (^[req app] (session-ref)))
  (with-session
   (^[req app] (respond/ok req "Yo"))))

(define-http-handler #/.*/
  (^[req app] (respond/ok req "Huh?")))
