#!/usr/bin/env gosh

;;
;; Sample web server to handle post request.
;; Run it in the top source directory of Gauche-makiki.
;;

(add-load-path ".." :relative)

(use gauche.threads)
(use gauche.parseopt)
(use srfi-1)
(use makiki)

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port))
  0)

(define-http-handler "/"
  (^[req app]
    ($ respond/ok req
       '(sxml
         (html
          (head (title "Post test"))
          (body
           (form (@ (action "/post") (method "POST"))
                 (p "Type something:"
                    (input (@ (type "text") (name "text"))))
                 (input (@ (type "submit") (name "submit")
                           (value "post"))))))))))

(define-http-handler "/post"
  (with-post-parameters
   (^[req app]
     ($ respond/ok req
        `(sxml
          (html
           (head (title "Post test"))
           (body (p "You typed: " ,(request-param-ref req "text"))
                 (a (@ (href "/")) "Try again"))))))))

;; Local variables:
;; mode: scheme
;; end:


        

