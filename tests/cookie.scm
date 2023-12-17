(use gauche.parseopt)
(add-load-path ".." :relative)
(use makiki)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define-http-handler "/"
  (^[req app]
    (dolist [p (request-params req)]
      (response-cookie-add! req (car p) (cadr p) :max-age 60))
    (respond/ok req "OK")))
