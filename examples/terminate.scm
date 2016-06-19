#!/usr/bin/env gosh

(add-load-path ".." :relative)
(use gauche.parseopt)
(use makiki)

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port
                       :app-data (sys-ctime (sys-time)))))

(define-http-handler "/"
  (^[req app]
    (respond/ok req "<a href='/quit'>Quit</a>")))

(define-http-handler "/quit"
  (^[req app] (terminate-server-loop req 1)))

                     

