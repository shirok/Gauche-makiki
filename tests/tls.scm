(use gauche.parseopt)
(use file.util)
(add-load-path ".." :relative)
(use makiki)

(define *current-dir* (sys-dirname (current-load-path)))

(define (datapath file) (build-path *current-dir* file))

(define *tls-settings*
  `(:tls-certificates (,(datapath "data/test-cert.pem"))
    :tls-private-key ,(datapath "data/test-key.pem")
    :tls-private-key-password "cafebabe"))

(define (main args)
  (let-args (cdr args) ([p "port=i" 8081])
    (start-http-server :access-log #t :error-log #t
                       :tls-port p
                       :tls-settings *tls-settings*))
  0)
