(use gauche.parseopt)
(add-load-path ".." :relative)
(use makiki)

(define *ch* (make-server-control-channel))

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t
                       :control-channel *ch* :port p)))

(define-http-handler "/a"
  (^[req app] (terminate-server-loop *ch* 1) (respond/ok req "")))

(define-http-handler "/b"
  (^[req app] (terminate-server-loop *ch* 2) (respond/ok req "")))
