(add-load-path ".." :relative)
(use makiki)
(use gauche.parseopt)

;; SXML templating test
(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define *template*
  '(html (body (p ?greeting ", " ?name ".  Howzit?"))))

(define-http-handler "/"
  (^[req app]
    (let ([name     (request-param-ref req "name" :default "nobody")]
          [greeting (~ '#0=("Hello" "Aloha" "Yo" . #0#)
                       (request-param-ref req "g" :convert x->integer :default 0))])
      (respond/ok req `(sxml ,*template* (?greeting . ,greeting) (?name . ,name))))))
