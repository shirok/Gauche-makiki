#!/usr/bin/env gosh

;;
;; Sample of handling GET request
;;

(add-load-path ".." :relative)
(use gauche.threads)
(use gauche.parseopt)
(use srfi-1)
(use makiki)

;; Simple BBS.  The app-data contains a list of postings,
;; in ((name timestamp message) ...) format.

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port
                       :app-data (atom '())))
  0)

(define-http-handler "/"
  (^[req app]
    (let ([name    (request-param-ref req "name")]
          [message (request-param-ref req "message")])
      (when (and name message)
        (atomic-update! app (cut cons `(,name ,(sys-time) ,message) <>)))
      (respond/ok req (atomic app render-wall)))))

(define (render-wall msgs)
  (define (render-entry name timestamp message)
    `((dt (b ,name) " "
          "(",(sys-strftime "%Y/%m/%d %H:%M:%S" (sys-localtime timestamp))")")
      (dd ,message)))
  `(sxml
    (html
     (head (title "wall"))
     (body
      (form (@ (method "GET") (action "/"))
            (table (tr (th "Name:")
                       (td (input (@ (type "text") (name "name")))))
                   (tr (th "Message:")
                       (td (textarea (@ (name "message") (cols 60)) "")))
                   (tr (td)
                       (td (input (@ (type "submit") (name "s") (value "Write")))))))
      (dl ,@(append-map (pa$ apply render-entry) msgs))))))

;; Local variables:
;; mode: scheme
;; end:
