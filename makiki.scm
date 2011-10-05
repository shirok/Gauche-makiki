;;;
;;;   Copyright (c) 2010-2011 Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;    1. Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;    2. Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;
;;;    3. Neither the name of the authors nor the names of its contributors
;;;       may be used to endorse or promote products derived from this
;;;       software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-module makiki
  (use gauche.parameter)
  (use gauche.record)
  (use gauche.net)
  (use gauche.logger)
  (use gauche.selector)
  (use gauche.threads)
  (use control.job)
  (use control.thread-pool :prefix tpool:)
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use text.tree)
  (use file.util)
  (use rfc.822)
  (use rfc.uri)
  (use rfc.mime)
  (use text.html-lite)
  (use util.list)
  (use util.queue)
  (use util.match)
  (use www.cgi)
  (export start-http-server http-server-software
          access-log access-log-drain
          error-log error-log-drain
          request?
          request-socket request-iport request-oport
          request-path request-params request-headers request-header
          respond/ng respond/ok
          define-http-handler add-http-handler!
          file-handler)
  )
(select-module makiki)

;;;
;;; Some parameters
;;;

(define docroot (make-parameter "."))

(define http-server-software (make-parameter "gauche/makiki"))

;;;
;;; Logging
;;;

(define access-log-drain (make-parameter #f))
(define error-log-drain (make-parameter #f))

(define-macro (define-log-macro name drain-param)
  `(define-macro (,name fmt . args)
     (let1 drain (gensym)
       `(if-let1 ,drain (,,drain-param)
          (log-format ,drain ,fmt ,@args)))))

(define-log-macro access-log access-log-drain)
(define-log-macro error-log error-log-drain)

;;;
;;; Request packet
;;;

(define-record-type request  %make-request #t
  line                ; the first line of the request
  socket              ; client socket
  remote-addr         ; remote address (sockaddr)
  method              ; request method
  path                ; request path
  query               ; unparsed query string
  params              ; query parameters
  headers             ; request headers
  (status)            ; result status (set later)
  (response-headers)  ; response headers (set later)
  (response-size))    ; size of reply content in octets (set later)


(define-inline (make-request line socket method path query headers)
  (%make-request line socket (socket-getpeername socket)
                 method path (or query "")
                 (cgi-parse-parameters :query-string (or query ""))
                 headers #f '() 0))
(define-inline (make-partial-request msg socket)
  (%make-request #`"#<error - ,|msg|>" socket (socket-getpeername socket)
                 "" "" "" '() '() #f '() 0))

(define-inline (request-iport req) (socket-input-port (request-socket req)))
(define-inline (request-oport req) (socket-output-port (request-socket req)))

(define (request-header req header)
  (rfc822-header-ref (request-headers req) header))

;;;
;;; Generating response
;;;

(define *status-code-map*
  (hash-table 'eqv?
              '(100 . "Continue")
              '(101 . "Switching Protocols")
              '(200 . "OK")
              '(201 . "Created")
              '(202 . "Accepted")
              '(203 . "Non-Authoritative Information")
              '(204 . "No Content")
              '(205 . "Reset Content")
              '(206 . "Partial Content")
              '(300 . "Multiple Choices")
              '(301 . "Moved Permanently")
              '(302 . "Found")
              '(303 . "See Other")
              '(304 . "Not Modified")
              '(305 . "Use Proxy")
              '(306 . "(Unused)")
              '(307 . "Temporary Redirect")
              '(400 . "Bad Request")
              '(401 . "Unauthorized")
              '(402 . "Payment Required")
              '(403 . "Forbidden")
              '(404 . "Not Found")
              '(405 . "Method Not Allowed")
              '(406 . "Not Acceptable")
              '(407 . "Proxy Authentication Required")
              '(408 . "Request Timeout")
              '(409 . "Conflict")
              '(410 . "Gone")
              '(411 . "Length Required")
              '(412 . "Precondition Failed")
              '(413 . "Request Entity Too Large")
              '(414 . "Request-URI Too Long")
              '(415 . "Unsupported Media Type")
              '(416 . "Requested Range Not Satisfiable")
              '(417 . "Expectation Failed")
              '(500 . "Internal Server Error")
              '(501 . "Not Implemented")
              '(502 . "Bad Gateway")
              '(503 . "Service Unavailable")
              '(504 . "Gateway Timeout")
              '(505 . "HTTP Version Not Supported")
              ))

(define (%respond req code content-type content)
  (request-status-set! req code)
  (request-response-size-set! req (string-size content))
  (let ([port (request-oport req)]
        [desc (hash-table-get *status-code-map* code "")])
    (define (p x) (display x port))
    (define (crlf) (display "\r\n" port))
    (guard (e [(<system-error> e) (error-log "response error ~s" e)])
      (p "HTTP/1.1 ") (p code) (p " ") (p desc) (crlf)
      (p "Server: ") (p (http-server-software)) (crlf)
      (p "Content-Type: ") (p content-type) (crlf)
      (p "Content-Length: ") (p (string-size content)) (crlf)
      (crlf)
      (p content)
      (flush port))))

;; returns Request
(define (respond/ng req code :optional (keepalive #f))
  (%respond req code "text/plain; charset=utf-8"
            (hash-table-get *status-code-map* code ""))
  (unless keepalive (socket-close (request-socket req)))
  req)

;; returns Request
(define (respond/ok req body :optional (keepalive #f))
  (unwind-protect
      (guard (e [else (error-log "respond/ok error ~s" (~ e'message))
                      (respond/ng req 500)])
        (match body
          [(? string?) (%respond req 200 "text/html; charset=utf-8" body)]
          [('file filename)
           (let* ([ctype (rxmatch-case filename
                           [#/\.js$/ () "application/javascript; charset=uft-8"]
                           [#/\.png$/ () "image/png"]
                           [#/\.jpg$/ () "image/jpeg"]
                           [#/\.css$/ () "text/css"]
                           [#/\.html$/ () "text/html; charset=uft-8"]
                           [else "text/plain"])] ;ideally use file magic
                  [size (file-size filename)]
                  [content (and size (with-input-from-file filename
                                       (cut read-block size)))])
             (if content
               (%respond req 200 ctype content)
               (respond/ng req 404)))]
          [('plain obj) (%respond req 200 "text/plain; charset=utf-8"
                                  (write-to-string obj display))]
          [('json alist) (%respond req 200 "application/json; charset=uft-8"
                                   (alist->json alist))]
          [else (%respond req 200 "text/html; charset=utf-8"
                          (tree->string body))])
        req)
    (unless keepalive (socket-close (request-socket req)))))

(define (alist->json alist)
  (tree->string
   `("{",(intersperse
          ","
          (map (^p `(,(write-to-string (x->string (car p)))
                     ":",(item->json (cdr p))))
               alist))
     "}")))

(define (item->json item)
  (match item
    [#(elt ...)    `("[" ,@(intersperse "," (map item->json elt)) "]")]
    [((x . y) . _) (alist->json item)]
    [_ (write-to-string item)]))

;;;
;;; Handler mechanism
;;;

(define *handlers* (make-queue))

;; The server program registers appropriate handlers.
;;
;;  handler :: Request Path-RxMatch App -> IO ()
;;
;; The server test the request path against path-rx and calls matching handler.
;; A handler can be defined declaratively:
;;
;;   (define-http-handler path-rx handler)
;;
;; Or can be registered procedurally:
;;
;;   (add-http-handler! path-rx handler)

(define-syntax define-http-handler
  (syntax-rules ()
    [(_ path-rx handler) (add-http-handler! path-rx handler)]))

(define (add-http-handler! path-rx handler)
  (enqueue! *handlers* (cons path-rx handler)))

(define (dispatch-handler req app)
  (let1 path (request-path req)
    (or (any-in-queue (^p (cond [((car p) path) => (cut (cdr p) req <> app)]
                                [else #f]))
                      *handlers*)
        (respond/ng req 404))))

;;;
;;; Main loop
;;;

(define (start-http-server :key (host #f)
                                (port 8080)
                                (document-root ".")
                                (num-threads 5)
                                (max-backlog 10)
                                ((:access-log alog) #f)
                                ((:error-log elog) #f)
                                (app-data #f))
  ;; see initial-log-drain for the possible values of :access-log/:error-log.
  (parameterize ([access-log-drain (initial-log-drain alog 'access-log)]
                 [error-log-drain (initial-log-drain elog 'error-log)]
                 [docroot document-root])
    (let* ([pool (tpool:make-thread-pool num-threads :max-backlog max-backlog)]
           [tlog (kick-logger-thread pool)]
           [ssocks (make-server-sockets host port :reuse-addr? #t)])
      (unwind-protect
          (let1 sel (make <selector>)
            (dolist [s ssocks]
              (selector-add! sel (socket-fd s)
                             (lambda (fd condition)
                               (accept-client app-data (socket-accept s) pool))
                             '(r)))
            (access-log "~a: started on ~a" (logtime (current-time))
                        (map (.$ sockaddr-name socket-address) ssocks))
            (while #t (selector-select sel)))
        (access-log "~a: terminating" (logtime (current-time)))
        (for-each socket-close ssocks)
        (tpool:terminate-all! pool :force-timeout 300)
        (thread-terminate! tlog)))))

(define (kick-logger-thread pool)
  (thread-start! (make-thread (cut logger pool))))

(define (accept-client app csock pool)
  (unless (tpool:add-job! pool (cut handle-client app csock) #t)
    (respond/ng (make-partial-request "too many request backlog" csock) 503)
    (socket-close csock)))

(define (handle-client app csock)
  (guard (e [else (respond/ng (make-partial-request (~ e'message) csock) 500)])
    (let* ([iport (socket-input-port csock)]
           [line (read-line (socket-input-port csock))])
      (rxmatch-case line
        [test eof-object?
         (respond/ng (make-partial-request "client gone" csock) 400)]
        [#/^(GET|HEAD|POST)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
         (receive (auth path q frag) (uri-decompose-hierarchical abs-path)
           (let1 path (uri-decode-string path :cgi-decode #t)
             (dispatch-handler (make-request line csock meth path q
                                             (rfc822-read-headers iport))
                               app)))]
        [#/^[A-Z]+.*/ ()
          (respond/ng (make-request line csock "" "" "" '()) 501)]
        [else (respond/ng (make-request line csock "" "" "" '()) 400)]))))

;;;
;;; Logging
;;;

;; used in initialization to construct a log drain from a keyword arg
;; to start-http-server.
;; DEST can be #f (no log), #t (stdout), string (filename) or <log-drain>.
;; For access log, <log-drain> is better not to have prefix, for timestamp
;; is included in the message.
(define (initial-log-drain dest kind)
  (cond [(not dest) #f]
        [(is-a? dest <log-drain>) dest]
        [else
         (make <log-drain> :path dest
               :prefix (case kind
                         [(access-log) ""]
                         [(error-log) (^_ #`",(logtime (current-time)): ")]
                         ))]))

(define (logger pool)
  (guard (e [else (error-log "[I] logger error: ~a" (~ e'message))])
    (let loop ()
      (let1 j (dequeue/wait! (~ pool'result-queue))
        (case (job-status j)
          [(done) (let1 r (job-result j)
                    (unless (request? r)
                      (error "some handler didn't return request:" r))
                    (access-log "~a: ~a \"~a\" ~a ~a ~s ~a"
                                (logtime (job-acknowledge-time j))
                                (logip (request-remote-addr r))
                                (request-line r)
                                (request-status r)
                                (request-response-size r)
                                (logreferer r)
                                (logdt (job-acknowledge-time j)
                                       (job-finish-time j))))]
          [(error) (error-log "[I] job error: ~a" (~ (job-result j)'message))]
          [(killed) (error-log "[I] job killed: ~a" (job-result j))]
          [else (error-log "[I] unexpected job status: ~a" (job-status j))]))
      (loop))
    (logger pool)))

(define (logtime time) (date->string (time-utc->date time) "~4"))

(define (logip addr)
  (inet-address->string (sockaddr-addr addr)
                        (case (sockaddr-family addr)
                          [(inet)  AF_INET]
                          [(inet6) AF_INET6]
                          [else AF_INET]))) ;just in case

(define (logdt t0 t1)
  (let1 dt (time-difference t1 t0)
    (format "~:d.~3,'0dms"
            (+ (* (time-second dt) 1000)
               (quotient (time-nanosecond dt) 1000000))
            (modulo (quotient (time-nanosecond dt) 1000) 1000))))

(define (logreferer req)
  (rfc822-header-ref (request-headers req) "referer" "-"))

;;;
;;; CGI adaptor
;;;

;; This can be used to call a cgi program's main procedure (PROC,
;; with setting cgi metavariables and current i/o's. 
;; We don't support http authentications yet.
(define (cgi-handler proc :key (script-name ""))
  (lambda (req m app)
    (with-input-from-port (request-iport req)
      (lambda ()
        (parameterize ((cgi-metavariables
                        (get-cgi-metavariables req script-name)))
          (proc ""))))))

;; Load file as a cgi script, and create a cgi handler that calls a
;; procedure named by ENTRY-POINT inside the script.
;; To avoid interference with makiki itself, the script is loaded
;; into an anonymous module.
;; Loading is done only once unless LOAD-EVERY-TIME is true.
;; Usually, loading only once cuts the overhead of script loading for
;; repeating requests.  However, if the cgi script sets some global
;; state, it should be loaded for every request---a script can
;; be executed concurrently, so any code relying on a shared mutable
;; global state will fail.
;; Note also that we assume the script itself isn't written inside
;; a specific module; if it has it's own define-module and
;; select-module, the module will be shared for every load, and
;; we won't have enough isolation.
(define (cgi-script file :key (entry-point 'main)
                              (script-name "")
                              (load-every-time #f))
  (define (load-script) ;returns entry point procedure
    (let1 mod (make-module #f)
      (load file :environment mod)
      (global-variable-ref mod entry-point)))

  (if load-every-time
    (lambda (req m app)
      ((cgi-handler (load-script) :script-name script-name) req m app))
    (cgi-handler (load-script) :script-name script-name)))
     
;; Sets up cgi metavariables.
(define (get-cgi-metavariables req script-name)
  (cond-list
   ;; AUTH_TYPE - not supported yet
   [(request-header req "content-length")
    => (^v (list "CONTENT_LENGTH" (x->string v)))]
   [(request-header req "content-type")
    => (^v (list "CONTENT_LENGTH" (x->string v)))]
   [#t `("GATEWAY_INTERFACE" "CGI/1.1")]
   [#t `("PATH_INFO" ,(request-path req))]
   [#t `("PATH_TRANSLATED" ;todo - flexible path trans.
         ,(string-append (docroot) (request-path req)))]
   [#t `("QUERY_STRING" ,(request-query req))]
   [#t `("REMOTE_ADDR" ,(logip (request-remote-addr req)))]
   [#t `("REMOTE_HOST"  ,(request-remote-addr req))]
   ;; REMOTE_IDENT - not supported
   ;; REMOTE_USER - not supported
   [#t `("REQUEST_METHOD" ,(request-method req))]
   [#t `("SCRIPT_NAME" ,script-name)]
   ;; NB: for HTTP/1.1, the upper layer should reject the request if host
   ;; header isn't present.  But just in case:
   [#t `("SERVER_NAME" ,(or (request-header req "host") (sys-gethostname)))]
   [#t `("SERVER_PORT" ,(x->string (sockaddr-port (socket-getsockname
                                                   (request-socket req)))))]
   [#t `("SERVER_PROTOCOL" "HTTP/1.1")]
   [#t `("SERVER_SOFTWARE" ,(http-server-software))]
   ))  

;;;
;;; Built-in file handler
;;;

(define (file-handler :key (directory-index '("index.html" #t)))
  (lambda (req m app) (%handle-file req m directory-index)))

(define (%handle-file req m dirindex)
  (let1 rpath (sys-normalize-pathname (request-path req) :canonicalize #t)
    (if (or (string-prefix? "/../" rpath)
            (string=? "/.." rpath))
      (respond/ng req 403)      ;do not allow path traversal
      (let1 fpath (sys-normalize-pathname #`",(docroot),rpath")
        (cond [(file-is-readable? fpath)
               (if (file-is-directory? fpath)
                 (%handle-directory req fpath rpath dirindex)
                 (respond/ok req `(file ,fpath)))]
              [(file-exists? fpath) (respond/ng req 403)]
              [else (respond/ng req 404)])))))

(define (%handle-directory req fpath rpath dirindex)
  (let loop ([ind dirindex])
    (match ind
      [() (respond/ng req 403)]
      [(#t . _) (respond/ok req (%index-directory fpath rpath))]
      [(name . rest) (let1 f (build-path fpath name)
                       (if (file-is-readable? f)
                         (respond/ok req `(file ,f))
                         (loop rest)))])))

(define (%index-directory fpath rpath)
  (receive (dirs files) (directory-list2 fpath)
    (html:html
     (html:head (html:title rpath))
     (html:body
      (html:h1 rpath)
      (html:hr)
      (html:ul
       (map (cut %render-file-entry <> rpath "/") dirs)
       (map (cut %render-file-entry <> rpath "") files))))))

(define (%render-file-entry name rpath suffix)
  (html:li
   (html:a :href (build-path rpath name)
           (html-escape-string (string-append name suffix)))))
