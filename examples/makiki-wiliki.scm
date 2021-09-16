#!/usr/bin/env gosh

;; Sample to run WiLiKi https://practical-scheme.net/wiliki/wiliki.cgi
;; inside Makiki.

(use makiki)
(use makiki.cgi)
(use gauche.parseopt)

(add-load-path "." :relative) ; to load makiki-wiliki.cgi

;; Makiki's main routine.
;; After starting up the server, you can access wiliki with
;; <http://localhost:8521/wiliki.cgi>
;; You may want to adjust log output.
(define (main args)
  (let-args (cdr args) ([p "port=i" 8521])
    (start-http-server :access-log #t
                       :error-log "makiki-wiliki.log"
                       :port p))
  0)

;; This delegates access to "/wiliki.cgi" to the 'main' routine of
;; makiki-wiliki.cgi.
;; :forwarded #t allows this makiki server to run behind reverse proxy.
(define-http-handler #/^\/wiliki.cgi(\/.*)?/
  (cgi-script "makiki-wiliki.cgi"
              :script-name "/wiliki.cgi" :forwarded #t))

;; If you serve static content via Makiki, add file-handler for them.
(define-http-handler "/wiliki.css" (file-handler))

;; Reverse Proxy Settings on Apache2
;;
;;  You can run this makiki server behind reverse proxy.
;;
;;  1. Enable mod_proxy and mod_headers, if you haven't.
;;
;;  2. Add ProxyPass, ProxyPassReverse, and RequestHeader directives
;;     in the Apache conf file:
;;
;;  <VirtualHost *:443>
;;     ...
;;     ProxyPass         /makiki-wiliki/ http://localhost:8521/ retry=0
;;     ProxyPassReverse  /makiki-wiliki/ http://localhost:8521/
;;     RequestHeader set X-Forwarded-Proto https
;;  </VirtualHost>
;;  <VirtualHost *:80>
;;     ...
;;     ProxyPass         /makiki-wiliki/ http://localhost:8521/ retry=0
;;     ProxyPassReverse  /makiki-wiliki/ http://localhost:8521/
;;  </VirtualHost>
;;
;;
;;  After restarting the apache and running makiki-wiliki.scm on port
;;  8521, you can access it with <https://yourserver/makiki-wiliki/wiliki.cgi>
;;
