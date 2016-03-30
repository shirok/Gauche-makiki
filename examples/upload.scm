#!/usr/bin/env gosh

;;
;; Sample web server to handle file upload via post request.
;; Run it in the top source directory of Gauche-makiki.
;;

;; NB: Gauche up to 0.9.3.3 has a bug handling multiple file uploads;
;; use 0.9.4.

(add-load-path ".." :relative)
(use gauche.threads)
(use gauche.parseopt)
(use srfi-1)
(use file.util)
(use makiki)

(define *upload-prefix* "/tmp/upload-sample-")

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port))
  0)

(define-http-handler "/"
  (^[req app]
    ($ respond/ok req
       '(sxml
         (html
          (head (title "Upload test"))
          (body
           (form (@ (action "/upload") (method "POST")
                    (enctype "multipart/form-data"))
                 (p "Choose file(s) to upload:"
                    (input (@ (type "file") (name "files")
                              (multiple "multiple"))))
                 (input (@ (type "submit") (name "submit")
                           (value "post"))))))))))

(define-http-handler "/upload"
  (with-post-parameters
   (^[req app]
     (let-params req ([tnames "q:files" :list #t])
       ($ respond/ok req
          `(sxml
            (html
             (head (title "Upload test"))
             (body
              (p "Uploaded files:")
              (table
               (@ (border "1"))
               (tr (th "tmp name") (th "original name") (th "file size"))
               ,@(map
                  (^[tmp&orig]
                    `(tr (td (tt ,(car tmp&orig)))
                         (td (tt ,(cadr tmp&orig)))
                         (td (tt ,(x->string (file-size (car tmp&orig)))))))
                  tnames))))))))
   :part-handlers `(("files" file+name :prefix ,*upload-prefix*))))

;; Local variables:
;; mode: scheme
;; end:


        

