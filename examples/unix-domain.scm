(use makiki)
(define (main args) (start-http-server :path #"/tmp/makiki~(sys-getpid)"
                                       :access-log #t
                                       :error-log #t))
(define-http-handler "/"
  (^[req app] (respond/ok req "<h1>Running over a unix domain socket.</h1>")))


