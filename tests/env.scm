;; A test for cgi-handler.
;; This script is loaded into makiki server

(use srfi-98)
(use www.cgi)
(use text.html-lite)

(define (main args)
  (cgi-main
   (^[params]
     `(,(cgi-header :content-type "text/html; charset=uft-8")
       ,(html:html
         (html:head (html:title "test"))
         (html:body
          (html:h2 "Parameters")
          (html:ul
           (map (^p (html:li (html-escape-string (car p))
                             " = "
                             (html-escape-string (x->string (cdr p)))))
                params))
          (html:h2 "Environment")
          (html:ul
           (map (^p (html:li (html-escape-string (car p))
                             " = "
                             (html-escape-string (x->string (cdr p)))))
                (get-environment-variables)))))))))

           

            
                                                          
                          