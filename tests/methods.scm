(use gauche.parseopt)
(add-load-path ".." :relative)
(use makiki)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define-http-handler (GET)  "/" (^[req app] (respond/ok req "get")))
(define-http-handler (POST) "/" (^[req app] (respond/ok req "post")))
(define-http-handler (PUT)  "/" (^[req app] (respond/ok req "put")))
(define-http-handler (DELETE) "/" (^[req app] (respond/ok req "delete")))
