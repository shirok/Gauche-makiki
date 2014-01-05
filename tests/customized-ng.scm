;; Returning customized body with non-200 response

(use makiki)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ([p "port=i"])
    (start-http-server :access-log #t :error-log #t :port p))
  0)

(define-http-handler "/"
  (^[req app]
    (respond/ng req 404
                :body '(sxml (html (title "404 Not found")
                                   (body (p "I don't have that!")))))))

;; This triggers 404 situation inside respond/ng, recursing to respond/ng.
;; See if we won't enter infinite loop.
(define-http-handler "/favicon.ico"
  (^[req app]
    (respond/ng req 404 :body '(file "no favicon!"))))

