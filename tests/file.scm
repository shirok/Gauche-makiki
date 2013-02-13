(use makiki)
(use gauche.parseopt)

(define-http-handler #/^\/.*$/ (file-handler))

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t
                       :error-log "test-file.log"
                       :port p))
  0)
