#!/usr/bin/env gosh

;; A CGI script to be used from cgiserv.scm.

(use www.cgi)
(use text.html-lite)

(define *metavariables*
  '(AUTH_TYPE
    CONTENT_LENGTH
    CONTENT_TYPE
    GATEWAY_INTERFACE
    HTTPS
    PATH_INFO
    PATH_TRANSLATED
    QUERY_STRING
    REMOTE_ADDR
    REMOTE_HOST
    REMOTE_IDENT
    REMOTE_USER
    REQUEST_METHOD
    SCRIPT_NAME
    SERVER_NAME
    SERVER_PORT
    SERVER_PROTOCOL
    SERVER_SOFTWARE))

(define (main args)
  (cgi-main
   (^[params]
     `(,(cgi-header)
       ,(html-doctype)
       ,(html:html
         (html:head (html:title "cgienv"))
         (html:body
          (html:h1 "CGI metavariables")
          (html:table
           (map (^m
                 (html:tr
                  (html:th (html:tt (x->string m)))
                  (html:td (html:tt (html-escape-string
                                     (x->string
                                      (cgi-get-metavariable (x->string m))))))))
                *metavariables*))
          (html:h1 "Query parameters")
          (html:tt (html-escape-string (write-to-string params)))
          ))))))

;; Local variables:
;; mode: scheme
;; end:
