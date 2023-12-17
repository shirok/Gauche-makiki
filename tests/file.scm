(use gauche.parseopt)
(add-load-path ".." :relative)
(use makiki)

(define-http-handler #/^\/.*$/
  (file-handler :root (sys-dirname (current-load-path))))

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t
                       :error-log "test-file.log"
                       :port p))
  0)
