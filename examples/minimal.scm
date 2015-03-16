(use makiki)
(define (main args) (start-http-server :port 6789))
(define-http-handler "/"
  (^[req app] (respond/ok req "<h1>It worked!</h1>")))

