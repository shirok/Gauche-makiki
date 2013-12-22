#!/usr/bin/env gosh

;;
;; Sample web server to show how to use makiki.  Run it in the
;; top source directory of Gauche-makiki.
;;

(add-load-path ".." :relative)
(use gauche.threads)
(use gauche.parseopt)
(use text.html-lite)
(use srfi-1)
(use makiki)

;; main program just starts the server.
;; logs goes to stdout (:access-log #t :error-log #t)
;; we pass the timestamp to :app-data - it is avaliable to the 'app'
;;  argument in the http handlers below.
(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (start-http-server :access-log #t :error-log #t :port port
                       :app-data (sys-ctime (sys-time))))
  0)

;; The root path handler.  We show some html, constructed by text.html-lite.
(define-http-handler "/"
  (^[req app]
    (respond/ok req
      (html:html
       (html:head (html:title "Makiki"))
       (html:body (html:h1 "You're running Makiki http server")
                  (html:p "The server is running since " app
                          "at port " (request-server-port req)
                          " on host " (request-server-host req)
                          ".")
                  (html:p
                   (html:a :href "/src/" "Browse makiki source"))
                  (html:p
                   (html:a :href "/echo-headers" "View request headers")))))))

;; The path '/src/' shows the current directory and below.
;; We pass the proc to extract path below '/src' to the :path-trans
;; arg of file-handler, which will interpret the translated path relative
;; to the document-root, which defaults to ".".
(define-http-handler #/^\/src(\/.*)$/
  (file-handler :path-trans (^[req] ((request-path-rxmatch req) 1))))

;; Redirect "/src" to "/src/".
(define-http-handler "/src"
  (^[req app] (respond/redirect req "/src/")))

;; '/echo-header' reports back http request headers, handy for diagnostics.
(define-http-handler "/echo-headers"
  (^[req app]
    (respond/ok req
                (html:html
                 (html:head (html:title "echo-header"))
                 (html:body (html:h1 "Request headers")
                            (html:pre
                             (map (^p (map (^v #`",(car p): ,v\n") (cdr p)))
                                  (request-headers req))))))))

;; Local variables:
;; mode: scheme
;; end:
