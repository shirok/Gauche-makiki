(use gauche.parseopt)
(use gauche.uvector)
(use rfc.json)
(add-load-path ".." :relative)
(use makiki)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

;; Receives POST with json {"count":n}, and returns json with {"count":n+1}

(define-http-handler "/"
  (with-post-json
   (^[req app]
     (if-let1 json (request-param-ref req "json-body")
       (respond/ok req `(json (("count" . ,(+ 1 (assoc-ref json "count" 0))))))
       (respond/ok req '(json (("count" . 0))))))))
