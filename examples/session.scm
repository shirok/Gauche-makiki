#!/usr/bin/env gosh
;; Authentication and session management example for Gauche 0.9.4
;; It is simpler with Gauche 0.9.5 and later.  See session2.scm.
;; Run this script in the top directory of Gauche-makiki

(add-load-path ".." :relative)
(use gauche.parseopt)
(use gauche.threads)
(use text.html-lite)
(use util.match)
(use srfi-27)
(use makiki)

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8012])
    (random-source-randomize! default-random-source)
    (start-http-server :access-log #t :error-log #t :port port
                       ;; We pass session hashtable as app-data
                       :app-data (atom (make-hash-table 'equal?))))
  0)

(define *password-db*
  ;; ((user . pass) ..)
  '(("ravel" . "maurice")
    ("debussy" . "claude")
    ("faure" . "gabriel")))

;; Simple session management
;; Here we just use a hashtable that maps session id to (timestamp . data)
;; It is in-memory, so server restarts would invalidate it.
;; In real app, you can use some persistence (e.g. dbm) and/or external
;; database (dbi, memchache, etc.)
;; NB: This should be simpler.

;; Session times out in 10 minutes
(define (%sweep-session table)
  (define cutoff (- (sys-time) (* 10 60)))
  (hash-table-for-each table
                       (^[key entry]
                         (when (< (car entry) cutoff)
                           (hash-table-delete! table key)))))

(define (session-data req app)
  (let-params req ([key "c:sess"])
    (and key
         ($ atomic app
            (^[table]
              (%sweep-session table)
              (and-let* ([p (hash-table-get table key #f)])
                (cdr p)))))))

(define (session-create! req app data)
  ($ atomic app
     (^[table]
       (%sweep-session table)
       (let* ([now (sys-time)]
              [key (format "~8,'0x~16,'0x" now (random-integer (expt 2 64)))])
         (hash-table-put! table key (cons now data))
         (response-cookie-add! req "sess" key :path "/")))))

(define (session-delete! req app)
  (let-params req ([key "c:sess"])
    (and key (atomic app (^[table] (hash-table-delete! table key))))))

;; session-data :
;;  (#f . <path>)     - a non-logged-in client is trying to access <path>
;;  (#t . <username>) - logged in.

;;
;; Routing
;;

;; Returns username if the client has active session, #f otherwise.
(define (check-login req app)
  (and-let* ([data (session-data req app)]
             [ (car data) ])
    (cdr data)))

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

(define-http-handler ("src" . path)
  ? check-login
  ;; see sample-server for explanation of :path-trans
  (file-handler :path-trans (^[req] #"/~(request-path-ref req 'path)")))

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
