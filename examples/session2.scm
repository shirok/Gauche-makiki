#!/usr/bin/env gosh
;; Authentication and session management example for Gauche 0.9.5 and later.
;; Run this script in the top directory of Gauche-makiki

(add-load-path ".." :relative)
(use gauche.parseopt)
(use gauche.threads)
(use text.html-lite)
(use util.match)
(use data.cache)
(use srfi-27)
(use makiki)

;; application
(define-class <app> ()
  (;; session table is just a TTLR cache, keyed by session token in the cookie.
   ;; session data :
   ;;  (#f . <path>)     - a non-logged-in client is trying to access <path>
   ;;  (#t . <username>) - logged in.
   [sessions :init-form (make-ttlr-cache (* 10 60))]))

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (random-source-randomize! default-random-source)
    (start-http-server :access-log #t :error-log #t :port port
                       :app-data (atom (make <app>))))
  0)

(define *password-db*
  ;; ((user . pass) ..)
  '(("ravel" . "maurice")
    ("debussy" . "claude")
    ("faure" . "gabriel")))

;; Returns session-data (#f . path) or (#t . user), if it exists.
(define (session-data req app)
  (let-params req ([cookie "c:sess"])
    (and cookie
         (atomic app (^a (cache-lookup! (~ a'sessions) cookie #f))))))

;; Returns username if the client has active session, #f otherwise.
(define (check-login req app)
  (and-let* ([data (session-data req app)]
             [ (car data) ])
    (cdr data)))

;; Delete session
(define (session-delete! req app)
  (let-params req ([cookie "c:sess"])
    (and cookie
         (atomic app (^a (cache-evict! (~ a'sessions) cookie))))))

;; Create a new session
(define (session-create! req app data)
  (let1 key (format "~8,'0x~16,'0x" (sys-time) (random-integer (expt 2 64)))
    ($ atomic app (^a (cache-write! (~ a'sessions) key data)))
    (response-cookie-add! req "sess" key :path "/")))

;;
;; Routing
;;

;; '/' and '/src/' are accessible after login.
(define-http-handler "/"
  ? check-login
  (^[req app]
    ($ respond/ok req
       (html:html
        (html:head (html:title "Session sample"))
        (html:body
         (html:p "Hello, " (html-escape-string (request-guard-value req)) "!")
         (html:p (html:a :href "/src/" "Browse makiki source"))
         (html:p (html:a :href "/logout" "Log out")))))))

(define-http-handler #/^\/src(\/.*)$/
  ? check-login
  ;; see sample-server for explanation of :path-trans
  (file-handler :path-trans (^[req] ((request-path-rxmatch req) 1))))

(define-http-handler "/src"
  (^[req app] (respond/redirect req "/src/")))

;; Browsers may try to fetch this.  We catch this specially so that
;; the later 'catch all' clause won't be confused.
(define-http-handler "/favicon.ico" (^[req app] (respond/ng req 404)))

;; Logout
(define-http-handler "/logout"
  (^[req app]
    (session-delete! req app)
    ($ respond/ok req
       (html:html
        (html:head (html:title "Logged out"))
        (html:body (html:p "You've successfully logged out.")
                   (html:p (html:a :href "/" "login")))))))

;; Login check
(define-http-handler "/login"
  ? session-data
  (with-post-parameters
   (^[req app]
     (match (request-guard-value req)
       [(and (#f . path) data)
        (let-params req ([u "q:user"]
                         [p "q:pass"])
          (if (equal? (assoc-ref *password-db* u) p)
            (begin
              (set! (car data) #t)
              (set! (cdr data) u)
              (respond/redirect req path))
            (respond/ok req (login-form "Invalid login"))))]
       [(#t . user)
        ($ respond/ok req
           (html:html
            (html:head (html:title "Welcome"))
            (html:body (html:p "You've already logged in.")
                       (html:p (html:a :href "/" "Top"))
                       (html:p (html:a :href "/logout" "Log out")))))]))))

;; We use 'catch all' pattern, so that any req that hasn't match
;; previous patterns comes here.
(define-http-handler #/^.*$/
  (^[req app]
    (session-create! req app `(#f . ,(request-path req)))
    (respond/ok req (login-form #f))))

(define (login-form msg)
  (html:html
   (html:head (html:title "Login"))
   (html:body (if msg (html:p msg) "")
              (html:form
               :action "/login" :method "POST"
               (html:p "Username:" (html:input :type "text" :name "user"))
               (html:p "Password:" (html:input :type "password" :name "pass"))
               (html:input :type "submit" :name "submit" :value "Login")))))

;; Local variables:
;; mode: scheme
;; end:
