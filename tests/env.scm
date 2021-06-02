;; A test for cgi-handler.
;; This script is loaded into makiki server

(use srfi-98)
(use www.cgi)
(use text.html-lite)

(define (main args)
  (cgi-main
   (^[params]
     `(,(cgi-header :content-type "text/plain; charset=uft-8")
       ,(write-to-string
         `((parameters ,@params)
           (environments ,@(get-environment-variables))))))))
