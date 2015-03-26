;;;
;;;   Copyright (c) 2010-2013 Shiro Kawai  <shiro@acm.org>
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
  (use gauche.uvector)
  (use gauche.generator)
  (use gauche.vport)
  (use control.job)
  (use control.thread-pool :prefix tpool:)
  (use srfi-13)
  (use srfi-19)
  (use text.tree)
  (use file.util)
  (use rfc.822)
  (use rfc.uri)
  (use rfc.mime)
  (use rfc.cookie)
  (use rfc.json)
  (use text.html-lite)
  (use util.queue)
  (use util.match)
  (use sxml.tools)
  (use www.cgi)
  (export start-http-server http-server-software
          access-log access-log-drain
          error-log error-log-drain
          add-method-dispatcher!
          request? request-socket request-iport request-oport
          request-line request-method request-uri request-http-version
          request-server-host request-server-port
          request-path request-path-rxmatch request-guard-value
          request-response-error
          request-params  request-param-ref
          request-headers request-header-ref
          request-cookies request-cookie-ref
          respond/ng respond/ok respond/redirect
          response-header-push! response-header-delete!
          response-header-replace!
          response-cookie-add! response-cookie-delete!
          define-http-handler add-http-handler!
          document-root
          file-handler file-mime-type cgi-handler cgi-script
          with-header-handler with-post-parameters)
  )
(select-module makiki)

;;;
;;; Some parameters
;;;

(define document-root (make-parameter "."))
(define http-server-software (make-parameter "gauche/makiki"))
(define file-mime-type (make-parameter (^[path] #f)))

;;;
;;; Logging
;;;

(define access-log-drain (make-parameter #f))
(define error-log-drain  (make-parameter #f))

;; NB: For access log, we want to use job-acknowledged-time for timestamp.
;; So we don't use :prefix feature of log-drain.  For public API, we attach
;; timestamp to format string.

;; NB: We make them macro, so that args won't be evaluated when unnecessary.
(define-syntax access-log
  (syntax-rules ()
    [(_ fmt args ...)
     (if-let1 drain (access-log-drain)
       (log-format drain (string-append "~a: " fmt)
                   (logtime (current-time)) args ...))]))

(define-syntax access-log-sans-ts  ; for internal use
  (syntax-rules ()
    [(_ fmt args ...)
     (if-let1 drain (access-log-drain)
       (log-format drain fmt args ...))]))

(define-syntax error-log
  (syntax-rules ()
    [(_ fmt args ...)
     (if-let1 drain (error-log-drain)
       (log-format drain fmt args ...))]))

;;;
;;; Request packet
;;;

;; Although many slots are mutable, most of them are set up before
;; application handler is called, and should be treated immutable.
(define-record-type request  %make-request #t
  ;; public slots
  line                ; request line
  socket              ; client socket
  remote-addr         ; remote address (sockaddr)
  method              ; request method (uppercase symbol)
  uri                 ; request uri
  http-version        ; http version (string, e.g. "1.1")
  server-host         ; request host (string)
  server-port         ; request port (integer)
  (path)              ; request path (string)
  (path-rxmatch)      ; #<regmatch> object of matched path (set by dispatcher)
  (guard-value)       ; the result of guard procedure (set by dispatcher)
  (query)             ; query string as passed in the request
  (params)            ; parsed query parameters
  (headers)           ; request headers
  (response-error)    ; #f if response successfully sent, #<error> otherwise.
                      ;  set by respond/* procedures.
  ;; private slots
  (cookies %request-cookies request-cookies-set!) ; promise of alist of parsed cookies
  (send-cookies)      ; alist of cookie spec (set by handler)
  (status)            ; result status (set by responder)
  (response-headers)  ; response headers (set by handler)
  (response-size))    ; size of reply content in octets (set by responder)

(define-inline (make-request request-line csock method request-uri
                             http-vers headers)
  (define host:port ($ rfc822-header-ref headers "host"
                       $ sockaddr-name $ socket-getsockname csock))
  (define-values (auth path query frag)
    (uri-decompose-hierarchical request-uri))
  (define-values (host port)
    (if-let1 m (#/:(\d+)$/ host:port)
      (values (m'before) (x->integer (m 1)))
      (values host:port 80)))
  (%make-request request-line csock (socket-getpeername csock)
                 method request-uri http-vers host port
                 (uri-decode-string path :cgi-decode #t) #f #f
                 query (cgi-parse-parameters :query-string (or query ""))
                 headers #f (delay (%request-parse-cookies headers))
                 '() #f '() 0))

(define-inline (make-ng-request msg socket)
  (%make-request msg socket (socket-getpeername socket) ""
                 "" "" "" 80 "" #f #f "" '() '() #f '() '() #f '() 0))

;; API
(define-inline (request-iport req) (socket-input-port (request-socket req)))

;; API
(define-inline (request-oport req) (socket-output-port (request-socket req)))

;; some convenience accessors
;; API
(define (request-param-ref req param-name . keys)
  (apply cgi-get-parameter param-name (request-params req) keys))

;; API
(define (request-header-ref req header-name :optional (default #f))
  (rfc822-header-ref (request-headers req) header-name default))

;; API
(define (response-header-push! req header-name value)
  (push! (request-response-headers req) (list header-name value)))

;; API
(define (response-header-delete! req header-name)
  (update! (request-response-headers req)
           (cut remove (^e (equal? (car e) header-name)) <>)))

;; API
(define (response-header-replace! req header-name value)
  (response-header-delete! req header-name)
  (response-header-push! req header-name value))

;; Cookie utilities
(define (%request-parse-cookies headers)
  (parse-cookie-string (or (rfc822-header-ref headers "cookie")
                           (rfc822-header-ref headers "cookie2")
                           "")))

;; API
(define (request-cookies req) (force (%request-cookies req)))

;; API
(define (request-cookie-ref req name :optional (default #f))
  (or (assoc name (request-cookies req)) default))

;; API
(define (response-cookie-add! req name value . options)
  (response-cookie-delete! req name)
  (push! (request-send-cookies req) `(,name ,value ,@options)))

;; API
(define (response-cookie-delete! req name)
  (update! (request-send-cookies req)
           (cut remove (^e (equal? (car e) name)) <>)))

;;;
;;; Generating response
;;;

;; this will be unnecessary after Gauche 0.9.5
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

;; <content> : <chunk> | (<size> <chunk> ...)
;; <chunk>   : <string> | <u8vector>
;;
;; If the chunk is a list, the first element <size> must be the size of
;; the entire output.  (In future, we allow #f here and use chunked output.)
;; NB: taking advantage of the lazy sequence in Gauche 0.9.3, you can
;; lazily feed the file content to the client.
(define (%respond req code content-type content)
  (request-status-set! req code)
  ($ request-response-size-set! req
     (cond [(string? content) (string-size content)]
           [(u8vector? content) (u8vector-length content)]
           [(pair? content) (car content)]
           [else (error "invalid response content:" content)]))
  (let ([port (request-oport req)]
        [desc (hash-table-get *status-code-map* code "")])
    (define (p x) (if (u8vector? x) (write-block x port) (display x port)))
    (define (crlf) (display "\r\n" port))
    (guard (e [(and (<system-error> e) (eqv? (~ e'errno) EPIPE))
               (error-log "response error ~s: ~a" (class-name (class-of e))
                          (~ e'message))
               (close-output-port (request-oport req))
               (close-input-port (request-iport req))
               (set! (request-response-error req) e)])
      (p "HTTP/1.1 ") (p code) (p " ") (p desc) (crlf)
      (p "Server: ") (p (http-server-software)) (crlf)
      (p "Content-Type: ") (p content-type) (crlf)
      (p "Content-Length: ") (p (request-response-size req)) (crlf)
      (cond [(request-send-cookies req) pair?
             => (^[cs] ($ map (cut response-header-push! req "set-cookie" <>)
                          $ construct-cookie-string cs))])
      (dolist [h (request-response-headers req)]
        (dolist [v (cdr h)]
          (p (car h)) (p ": ") (p v) (crlf)))
      (crlf)
      (unless (eq? (request-method req) 'HEAD)
        (cond [(or (string? content) (u8vector? content)) (p content)]
              [(pair? content) (dolist [chunk (cdr content)] (p chunk))]))
      (flush port))))

;; Handle 'body' argument for respond/ok to respond/ng.  Returns
;; content-type and <content>.  If content-type arg is #f, we assume
;; the default content type, according to the BODY form.  Note that
;; 'file' body may ranse an condition 404 if the file doesn't exist.
;;
;; Supported forms:
;;  <string>
;;  <u8vector>
;;  (file <filename>)
;;  (plain <object>)
;;  (json <alist-or-vector>)
;;  (sxml <sxml>)
;;  (chunks <chunk> ...)
;;  <text-tree>
;;
(define (%response-body content-type body only-headers?)
  (let-syntax ([v (syntax-rules ()
                    [(_ ctype content)
                     (values (or content-type ctype) content)])])
    (match body
      [(? string?) (v "text/html; charset=utf-8" body)]
      [(? u8vector?) (v "application/binary" body)]
      [('file filename)
       (if-let1 content (%fetch-file-content filename (not only-headers?))
         (v (or ((file-mime-type) filename)
                (default-file-mime-type filename))
            content)
         (raise 404))]
      [('plain obj) (v "text/plain; charset=utf-8"
                       (write-to-string obj display))]
      [('json alist)(v "application/json; charset=utf-8"
                       (construct-json-string alist))]
      [('sxml node . substs)
       (let1 n (if (null? substs) node (%sxml-subst node substs))
         (match n
           [('html . _) (v "text/html; charset=utf-8"
                           (tree->string (sxml:sxml->html n)))]
           [_           (v "application/xml"
                           (tree->string (sxml:sxml->xml n)))]))]
      [('chunks . chunks)
       ;; NB: Once we support chunked output, we don't need to calculate
       ;; the total length.
       (v "application/octet-stream" ; take safe side for the default
          `(,(fold (^[c s]
                     (+ s (cond [(string? c) (string-size c)]
                                [(u8vector? c) (uvector-size c)]
                                [else (error "invalid chunk:" c)])))
                   0 chunks)
            ,@chunks))]
      [((? symbol? y) .  _) (error "invalid response body type:" y)]
      [else (v "text/html; charset=utf-8" (tree->string body))])))

;; Experimental: SXML template
;; TOOD: Repeating pattern
(define (%sxml-subst tree substs)
  (cond [(symbol? tree) (assq-ref substs tree tree)]
        [(pair? tree)
         (cons (car tree) (map (cut %sxml-subst <> substs) (cdr tree)))]
        [else tree]))

;; API
;; returns Request
;; If no-response, close connection immediately without sending response.
(define (respond/ng req code :key (keepalive #f) (no-response #f) (body #f)
                                  (content-type #f))
  (unless no-response
    (if body
      (guard (e [(integer? e) (respond/ng req e)]
                [else (error-log "respond/ng error ~s" (~ e'message))
                      (respond/ng req 500)])
        (receive (content-type content)
            ($ %response-body content-type
               (or body (hash-table-get *status-code-map* code "")) #f)
          (%respond req code content-type content)))
      (%respond req code "text/plain; charset=utf-8"
                (hash-table-get *status-code-map* code ""))))
  (unless (and keepalive (not no-response))
    (%socket-discard (request-socket req)))
  req)

;; API
;; returns Request
(define (respond/ok req body :key (keepalive #f) (content-type #f))
  (define headers-only? (eq? (request-method req) 'HEAD))
  (unwind-protect
      (guard (e [(integer? e) (respond/ng req e)]
                [else (error-log "respond/ok error ~s" (~ e'message))
                      (respond/ng req 500)])
        (receive (content-type content)
            (%response-body content-type body headers-only?)
          (%respond req 200 content-type content))
        req)
    (unless keepalive (%socket-discard (request-socket req)))))

;; Returns file contents as a lazy list of chunks, or #f
;; if we can't read the file.
(define-constant +chunk-size+ 65536)
(define (%fetch-file-content filename has-body?)
  (and-let* ([size (file-size filename)])
    (cond [(not has-body?) (list size)] ;let %respond use only the size info
          [(<= size +chunk-size+) (file->string filename)]
          [else
           ($ cons size $ generator->lseq
              $ file->generator filename (cut read-block +chunk-size+ <>))])))

;; API
;; Redirect.  URI can be an absolute uri or absolute path.
;; returns Request
(define (respond/redirect req uri :optional (code 302))
  (let1 target (uri-merge (uri-compose :scheme "http"
                                       :host (request-server-host req)
                                       :port (let1 p (request-server-port req)
                                               (if (= p 80) #f p)))
                          uri)
    (response-header-replace! req "location" target)
    (respond/ng req code)))

;;;
;;; Handler mechanism
;;;

(define *handlers* (make-mtqueue))

;; The server program registers appropriate handlers.
;;
;;  handler :: Request App -> IO ()
;;
;; The handler is responsible to process the request, and to call respond/*
;; procedure to send the response.
;;
;; Handlers can be registered by define-http-handler:
;;
;;   (define-http-handler pattern [? guard] handler)
;;
;; PATTERN is regexp or string.
;; GUARD is a procedure :: Request App -> Boolean.
;;
;; The server tries to match each pattern with the request path in the
;; order of registration.  When they match, and no guard procedure is given,
;; the corresponding handler is called.
;;
;; If the guard procedure is given in the matched handler, it is called
;; first; if it returns a true value, the handler is called; otherwise
;; the server keeps trying to match.
;;
;; Alternatively, the handler can registered procedurally:
;;
;;   (add-http-handler! path-rx handler [guard])

;; API
(define-syntax define-http-handler
  (syntax-rules (?)
    [(_ pattern ? guard handler) (add-http-handler! pattern handler guard)]
    [(_ pattern handler) (add-http-handler! pattern handler)]))

;; API
(define (add-http-handler! pattern handler :optional (guard (^[m a] #t)))
  (let1 rx (cond [(regexp? pattern) pattern]
                 [(string? pattern)
                  ($ string->regexp
                     $ string-append "^" (regexp-quote pattern) "$")]
                 [else (error "pattern must be a regexp or a string, but got"
                              pattern)])
    (enqueue! *handlers* (list rx guard handler))))

;; returns (handler req)
(define (find-handler path req app)
  (any-in-queue (^[entry]
                  (and-let* ([m ((car entry) path)]
                             [g (begin (request-path-rxmatch-set! req m)
                                       ((cadr entry) req app))])
                    (request-guard-value-set! req g)
                    (list (caddr entry) req)))
                *handlers*))

;;;
;;; Main loop
;;;

;; API
(define (start-http-server :key (host #f)
                                (port 8080)
                                ((:document-root docroot) ".")
                                (num-threads 5)
                                (max-backlog 10)
                                ((:access-log alog) #f)
                                ((:error-log elog) #f)
                                (forwarded? #f)
                                (app-data #f)
                                (startup-callback #f)
                                (shutdown-callback #f))
  ;; see initial-log-drain for the possible values of access-log and error-log.
  (parameterize ([access-log-drain (initial-log-drain alog 'access-log)]
                 [error-log-drain (initial-log-drain elog 'error-log)]
                 [document-root docroot])
    (let* ([pool (tpool:make-thread-pool num-threads :max-backlog max-backlog)]
           [tlog (kick-logger-thread pool forwarded?)]
           [ssocks (make-server-sockets host port :reuse-addr? #t)])
      (unwind-protect
          (let1 sel (make <selector>)
            (dolist [s ssocks]
              (selector-add! sel (socket-fd s)
                             (^[fd condition]
                               (accept-client app-data (socket-accept s) pool))
                             '(r)))
            (when startup-callback (startup-callback ssocks))
            (access-log "started on ~a"
                        (map (.$ sockaddr-name socket-address) ssocks))
            (while #t (selector-select sel)))
        (access-log "terminating")
        (for-each %socket-discard ssocks)
        (tpool:terminate-all! pool :force-timeout 300)
        (thread-terminate! tlog)
        (when shutdown-callback (shutdown-callback))))))

(define (kick-logger-thread pool forwarded?)
  (thread-start! (make-thread (cut logger pool forwarded?))))

(define (%socket-discard sock)
  (socket-close sock)
  (socket-shutdown sock SHUT_RDWR))

(define (accept-client app csock pool)
  (unless (tpool:add-job! pool (cut handle-client app csock) #t)
    (respond/ng (make-ng-request "[E] too many request backlog" csock) 503)
    (%socket-discard csock)))

(define (handle-client app csock)
  (guard (e [else
             ;; As of Gauche-0.9.4, there's an issue that report-error prints
             ;; nothing when called in a thread.  You need HEAD version of
             ;; Gauche to get stack dump.
             ;; We'll change report-error to take optional port argument by
             ;; 0.9.5 release.  Once done,
             ;; This can be just
             ;; (call-with-output-string (cut report-error e <>))
             (let1 trace (call-with-output-string
                           (^o (with-error-to-port o
                                 (cut report-error e))))
               (error-log "handle-client error ~s\n~a" (~ e'message) trace))
             (respond/ng (make-ng-request #"[E] ~(~ e'message)" csock) 500)])
    (let1 line (read-line (socket-input-port csock))
      (rxmatch-case line
        [test eof-object?
         (respond/ng (make-ng-request "(empty request)" csock) 400
                     :no-response #t)]
        [#/^(\w+)\s+(\S+)\s+HTTP\/(\d+\.\d+)$/ (_ meth req-uri httpvers)
         (let* ([method (string->symbol (string-upcase meth))]
                [headers (rfc822-read-headers (socket-input-port csock))]
                [req (make-request line csock method req-uri httpvers headers)])
           (if-let1 dispatcher (find-method-dispatcher method)
             (dispatcher req app)
             (respond/ng (make-ng-request #"[E] ~line" csock) 501)))]
        [else (respond/ng (make-ng-request #"[E] ~line" csock) 400)]))))

(define *method-dispatchers* (make-mtqueue))

(define (find-method-dispatcher method)
  (any-in-queue (^p (and (eq? (car p) method) (cdr p))) *method-dispatchers*))

;; API
(define (add-method-dispatcher! meth proc)
  (enqueue! *method-dispatchers* (cons meth proc)))

;; Default GET/HEAD/POST dispatcher
(define (%default-dispatch req app)
  (unwind-protect
      (match (find-handler (request-path req) req app)
        [(handler req) (handler req app)]
        [_ (respond/ng req 404)])
    ;; Clean temp files created by with-post-parameters
    ;; NB: We can use a parameter, assuming one thread handles
    ;; one request at a time.  If we introduce coroutines
    ;; (a thread may switch handling requests), we need to avoid
    ;; using cgi-temporary-files.
    (for-each sys-unlink (cgi-temporary-files))))

(add-method-dispatcher! 'GET  %default-dispatch)
(add-method-dispatcher! 'HEAD %default-dispatch)
(add-method-dispatcher! 'POST %default-dispatch)

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
                         [(error-log) (^_ #"~(logtime (current-time)): ")]
                         ))]))

(define (logger pool forwarded?)
  (guard (e [else (error-log "[I] logger error: ~a" (~ e'message))])
    (let loop ()
      (let1 j (dequeue/wait! (~ pool'result-queue))
        (case (job-status j)
          [(done) (let1 r (job-result j)
                    (unless (request? r)
                      (error "some handler didn't return request:" r))
                    ;; NB: This should be customizable!
                    ($ access-log-sans-ts "~a: ~a ~s ~a ~a ~s ~s ~a"
                       (logtime (job-acknowledge-time j))
                       (or (and forwarded?
                                (request-header-ref r "x-forwarded-for"))
                           (logip (request-remote-addr r)))
                       (request-line r)
                       (request-status r)
                       (request-response-size r)
                       (logreferer r)
                       (rfc822-header-ref (request-headers r)
                                          "user-agent" #f)
                       (logdt (job-acknowledge-time j)
                              (job-finish-time j))))]
          [(error) (error-log "[I] job error: ~a" (~ (job-result j)'message))]
          [(killed) (error-log "[I] job killed: ~a" (job-result j))]
          [else (error-log "[I] unexpected job status: ~a" (job-status j))]))
      (loop))
    (logger pool forwarded?)))

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

;; This can be used to call a cgi program's main procedure PROC,
;; with setting cgi metavariables and current i/o's. 
;; We don't support http authentications yet.
;;
;; SCRIPT-NAME should be an absolute path of the script that appear in
;; URL.  That is, if you want to make the script invoked as
;; http://example.com/foo/bar/baz.cgi, give "/foo/bar/baz.cgi".
;; It is necessary to calculate PATH_INFO properly.

(define (cgi-handler proc :key (script-name ""))
  (^[req app]
    (let1 out (open-output-string)
      (with-input-from-port (request-iport req)
        (^[]
          (let1 r (parameterize ([cgi-metavariables
                                  (get-cgi-metavariables req script-name)]
                                 [current-output-port
                                  (make <buffered-output-port>
                                    :flush (^[v f] (write-block v out)
                                                   (u8vector-length v)))])
                    (unwind-protect (proc `(,script-name))
                      (close-output-port (current-output-port))))
            (if (zero? r)
              (let* ([p    (open-input-string (get-output-string out))]
                     [hdrs (rfc822-read-headers p)])
                (dolist [h hdrs] (response-header-push! req (car h) (cadr h)))
                (if-let1 location (rfc822-header-ref hdrs "location")
                  (respond/redirect req location)
                  (respond/ok req (get-remaining-input-string p)
                              :content-type
                              (rfc822-header-ref hdrs "content-type"))))
              (respond/ng req 500))))))))

;; Load file as a cgi script, and create a cgi handler that calls a
;; procedure named by ENTRY-POINT inside the script.
;; To avoid interference with makiki itself, the script is loaded
;; into an anonymous module.
;;
;; See cgi-handler above for SCRIPT-NAME.  It's NOT the pathname to
;; the cgi script file.
;;
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
    (^[req app]
      ((cgi-handler (load-script) :script-name script-name) req app))
    (cgi-handler (load-script) :script-name script-name)))
     
;; Sets up cgi metavariables.
(define (get-cgi-metavariables req script-name)
  (cond-list
   ;; AUTH_TYPE - not supported yet
   [(request-header-ref req "content-length")
    => (^v (list "CONTENT_LENGTH" (x->string v)))]
   [(request-header-ref req "content-type")
    => (^v (list "CONTENT_LENGTH" (x->string v)))]
   [#t `("GATEWAY_INTERFACE" "CGI/1.1")]
   [#t `("PATH_INFO"
         ,(if (string-prefix? script-name (request-path req))
            (string-drop (request-path req)
                         (string-prefix-length script-name (request-path req)))
            (request-path req)))] ; not correct, but don't know what to do.
   [#t `("PATH_TRANSLATED" ;todo - flexible path trans.
         ,(string-append (document-root) (request-path req)))]
   [#t `("QUERY_STRING" ,(request-query req))]
   [#t `("REMOTE_ADDR" ,(logip (request-remote-addr req)))]
   [#t `("REMOTE_HOST"  ,(request-remote-addr req))]
   ;; REMOTE_IDENT - not supported
   ;; REMOTE_USER - not supported
   [#t `("REQUEST_METHOD" ,(x->string (request-method req)))]
   [#t `("SCRIPT_NAME" ,script-name)]
   [#t `("SERVER_NAME" ,(request-server-host req))]
   [#t `("SERVER_PORT" ,(request-server-port req))]
   [#t `("SERVER_PROTOCOL" "HTTP/1.1")]
   [#t `("SERVER_SOFTWARE" ,(http-server-software))]
   ))  

;;;
;;; Built-in file handler
;;;

;; API
(define (file-handler :key (directory-index '("index.html" #t))
                           (path-trans request-path))
  (^[req app] (%handle-file req directory-index path-trans)))

(define (%handle-file req dirindex path-trans)
  (let1 rpath (sys-normalize-pathname (path-trans req) :canonicalize #t)
    (if (or (string-prefix? "/../" rpath)
            (string=? "/.." rpath))
      (respond/ng req 403)      ;do not allow path traversal
      (let1 fpath (sys-normalize-pathname #"~(document-root)~rpath")
        (cond [(file-is-readable? fpath)
               ($ response-header-push! req "Last-modified"
                  (date->string ($ time-utc->date
                                   $ make-time time-utc 0 (file-mtime fpath))
                                "~a, ~d ~b ~Y ~X ~z"))
               (if (file-is-directory? fpath)
                 (%handle-directory req fpath rpath dirindex)
                 (respond/ok req `(file ,fpath)))]
              [(file-exists? fpath) (respond/ng req 403)]
              [else (respond/ng req 404)])))))

(define (%handle-directory req fpath rpath dirindex)
  (let loop ([ind dirindex])
    (match ind
      [() (respond/ng req 403)]
      [(#t . _)
       (respond/ok req (%index-directory (request-path req) fpath rpath))]
      [(name . rest) (let1 f (build-path fpath name)
                       (if (file-is-readable? f)
                         (respond/ok req `(file ,f))
                         (loop rest)))])))

(define (%index-directory showpath fpath rpath)
  (receive (dirs files) (directory-list2 fpath)
    (html:html
     (html:head (html:title showpath))
     (html:body
      (html:h1 showpath)
      (html:hr)
      (html:ul
       (map (cut %render-file-entry <> showpath "/") dirs)
       (map (cut %render-file-entry <> showpath "") files))))))

(define (%render-file-entry name rpath suffix)
  (html:li
   (html:a :href (build-path rpath name)
           (html-escape-string (string-append name suffix)))))

;; Built-in mime-type recognizer.
;; Applications can augument this by binding the file-mime-type parameter.
;; NB: Ideally we should try file magic instead of relying the suffix.
(define (default-file-mime-type path)
  (rxmatch-case path
    [#/\.js$/ () "application/javascript; charset=utf-8"]
    [#/\.png$/ () "image/png"]
    [#/\.(jpg|jpeg)$/ () "image/jpeg"]
    [#/\.gif$/ () "image/gif"]
    [#/\.css$/ () "text/css"]
    [#/\.(mpg|mpeg)$/ () "video/mpeg"]
    [#/\.(mp4)$/ () "video/mp4"]
    [#/\.(wm[xv]?)$/ (_ m) #"video/x-ms-~m"]
    [#/\.(wm[zd])$/ (_ m) #"application/x-ms-~m"]
    [#/\.(wma)$/ (_ m) #"audio/x-ms-~m"]
    [#/\.wav$/ () "audio/wav"]
    [#/\.(html|htm)$/ () "text/html; charset=utf-8"]
    [else "text/plain"])) ; fallback

;;;
;;; Adds header
;;;

;; API
;; header&values are keyword-value list.  Each keyword names header field,
;; and the value can be a string, a procedure that takes REQ and APP to
;; retrun a string value, or #f to omit the header.
(define (with-header-handler inner-handler . header&values)
  (^[req app]
    (dolist [h&v (slices header&values 2 :fill? #t)]
      (if-let1 val (cond
                    [(or (string? (cadr h&v)) (not (cadr h&v))) (cadr h&v)]
                    [(applicable? (cadr h&v) (class-of req) (class-of app))
                     ((cadr h&v) req app)]
                    [else (errorf "with-header-handler: Invalid header value \
                                   for header ~a: ~s" (car h&v) (cadr h&v))])
        (response-header-push! req (x->string (car h&v)) val)))
    (inner-handler req app)))

;;;
;;; Handling POST request
;;;

;; API
;; part-handlers are the same as cgi-parse-parameters
(define (with-post-parameters inner-handler :key (part-handlers '()))
  (^[req app]
    (when (eq? (request-method req) 'POST)
      (let1 params
          (parameterize ([cgi-metavariables
                          (cond-list
                           [(request-header-ref req "content-type")
                            => (cut list "CONTENT_TYPE" <>)]
                           [(request-header-ref req "content-length")
                            => (cut list "CONTENT_LENGTH" <>)]
                           [#t '("REQUEST_METHOD" "POST")])]
                         [current-input-port (request-iport req)])
            (cgi-parse-parameters :part-handlers part-handlers))
        (request-params-set! req params)))
    (inner-handler req app)))
