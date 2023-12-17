(use gauche.parseopt)
(add-load-path ".." :relative)
(use makiki)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define-http-handler #/^\/(?<path1>\w+)\/foo-(\d+)/
  (^[req app]
    (let-params req ([param1 "q" :default 'none]
                     [param2 "q:poo" :convert string->symbol :default #t]
                     [path1  "p"]
                     [path2  "p:2" :convert x->integer]
                     [header1 "h:user-agent"]
                     [header2 "h:x-header2" :convert read-from-string]
                     [cookie1 "c"]
                     [cookie2 "c" :convert x->integer :default -1])
      (respond/ok req
                  (write-to-string
                   (list param1 param2 path1 path2
                         header1 header2 cookie1 cookie2))
                  :content-type "text/plain"))))
