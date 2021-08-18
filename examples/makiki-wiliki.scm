#!/usr/bin/env gosh

;; Sample to run WiLiKi https://practical-scheme.net/wiliki/wiliki.cgi
;; inside Makiki.

(use makiki)
(use makiki.cgi)
(use gauche.parseopt)

;; makiki-wiliki.cgi contains the wiliki setting to be run as cgi script.
;; we just include the file, but into a separate module so that it won't
;; pollute user module.
(define-module makiki-wiliki
  (export (rename main main-wiliki))
  (include "makiki-wiliki.cgi"))
(import makiki-wiliki)

;; Makiki's main routine.
;; After starting up the server, you can access wiliki with
;; <http://localhost:8521/wiliki.cgi>
(define (main args)
  (let-args (cdr args) ([p "port=i" 8521])
    (start-http-server :access-log #t
                       :error-log "makiki-wiliki.log"
                       :port p))
  0)

;; This delegates access to "/wiliki.cgi" to the 'main' routine of
;; makiki-wiliki.cgi.
(define-http-handler "/wiliki.cgi"
  (cgi-handler main-wiliki :script-name "/wiliki.cgi"))

(define-http-handler "/wiliki.css" (file-handler))
