(use makiki)
(use gauche.parseopt)
(use gauche.uvector)
(use rfc.json)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

;; Receives POST with json {"count":n}, and returns json with {"count":n+1}

(define-http-handler "/"
  (^[req app]
    (let1 body (read-request-body req)
      (if (or (not body) (eof-object? body))
        (respond/ok req '(json (("count" . 0))))
        (let1 json (parse-json-string (u8vector->string body))
          (respond/ok req `(json (("count" . ,(+ 1 (assoc-ref json "count" 0)))))))))))

