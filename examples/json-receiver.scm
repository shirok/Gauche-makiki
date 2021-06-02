#!/usr/bin/env gosh

;;
;; Receives json via POST message and returns it in json,
;; as well as writes it out to stdout.  Useful to be a receiving
;; end of testing json post feature.
;;
;; Run it in the top source directory of Gauche-makiki.
;;

(add-load-path ".." :relative)
(use makiki)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port))
  0)

(define (handler req app)
  (let1 body (request-param-ref req "json-body" :default '())
    (display #"==== ~(sys-ctime (current-time))")
    (pprint body)
    (respond/ok req `(json ,body))))

(define-http-handler #/.*/ (with-post-json handler))
