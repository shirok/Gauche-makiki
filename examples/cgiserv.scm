#!/usr/bin/env gosh

;; Run cgienv.cgi inside makiki

(use makiki)
(use makiki.cgi)
(use gauche.parseopt)
(use file.util)

(deifne (main args)
  (let-args (cdr args) ([p "port=i" 8011])
    (start-http-server :access-log #t
                       :error-log #t
                       :port p))
  0)

(define-http-handler "/cgienv.cgi"
  (cgi-script (build-path (sys-dirname (current-load-path))
                          "cgienv.cgi")
              :script-name "/cgienv.cgi" :forwarded #t))
