#!/usr/bin/env gosh

;;
;; Receives request, dump it to stdout and return 200.
;; Useful for testing.
;;
;; Run it in the top source directory of Gauche-makiki.
;;

(add-load-path ".." :relative)
(use makiki)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #f :error-log #t :port port))
  0)

(define (handler req app)
  (print)
  (display #"==== ~(sys-ctime (current-time))")
  (print #"Method: ~(~ req'method)")
  (print #"URI: ~(~ req'uri)")
  (print #"Headers:")
  (pprint (~ req'headers))
  (and-let1 len (request-header-ref req "content-length")
    (let1 z (read-uvector <u8vector> len (request-iport req))
      (print "Body:")
      (display z)
      (print)))
  (respond/ok req "OK"))

(define-http-handler #/.*/ handler)
