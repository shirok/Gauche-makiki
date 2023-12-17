(use gauche.parseopt)
(use file.util)
(add-load-path ".." :relative)
(use makiki)
(use makiki.cgi)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t ;; required to make handshake work
                       :error-log "test-error-env2.log"
                       :port p))
  0)

(define dir (sys-dirname (current-load-path)))

(define-http-handler "/"
  (cgi-script (build-path dir "env.scm")))
