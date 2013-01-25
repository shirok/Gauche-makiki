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
  (use gauche.uvector)
  (use gauche.generator)
  (use gauche.vport)
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
  (use rfc.cookie)
  (use rfc.json)
  (use text.html-lite)
  (use util.list)
  (use util.queue)
  (use util.match)
  (use sxml.tools)
  (use www.cgi)
  (export start-http-server http-server-software
          access-log access-log-drain
          error-log error-log-drain
          request?
          request-socket request-iport request-oport request-method
          request-server-host request-server-port
          request-path request-path-rxmatch
          request-params request-response-error
          request-headers request-header-ref
          request-cookies request-cookie-ref
          respond/ng respond/ok respond/redirect
          response-header-push! response-header-delete!
          response-header-replace!
          response-cookie-add! response-cookie-delete!
          define-http-handler add-http-handler!
          file-handler cgi-handler cgi-script
          with-header-handler)
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
  ;; public slots
  line                ; the first line of the request
  socket              ; client socket
  remote-addr         ; remote address (sockaddr)
  method              ; request method (uppercase symbol)
  server-host         ; request host
  server-port         ; request port
  path                ; request path
  path-rxmatch        ; #<rxmatch> object of matched path
  query               ; unparsed query string
  params              ; query parameters
  headers             ; request headers
  (response-error)    ; #f if response successfully sent, #<error> otherwise.
                      ;  set by respond/* procedures.  The handler can check
                      ;  this slot and take actions in case of an error.
  ;; private slots
  (cookies %request-cookies) ; promise of alist of parsed cookies
  (send-cookies)      ; alist of cookie spec (set by handler)
  (status)            ; result status (set by responder)
  (response-headers)  ; response headers (set by handler)
  (response-size))    ; size of reply content in octets (set by responder)


(define-inline (make-request line socket method host:port path
                             rxmatch query headers)
  (rxmatch-let (#/^([^:]*)(?::(\d+))?$/ host:port) [_ h p]
    (%make-request line socket (socket-getpeername socket)
                   (string->symbol (string-upcase method))
                   h (if p (x->integer p) 80)
                   path rxmatch (or query "")
                   (cgi-parse-parameters :query-string (or query ""))
                   headers #f
                   (delay (%request-parse-cookies headers)) '()
                   #f '() 0)))

(define-inline (make-ng-request msg socket)
  (%make-request msg socket (socket-getpeername socket)
                 "" "" 80 "" #f "" '() '() #f '() '() #f '() 0))

;; API
(define-inline (request-iport req) (socket-input-port (request-socket req)))

;; API
(define-inline (request-oport req) (socket-output-port (request-socket req)))

;; some convenience accessors
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
  (request-response-size-set! req
                              (cond
                               [(string? content) (string-size content)]
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
        (cond
         [(or (string? content) (u8vector? content)) (p content)]
         [(pair? content) (dolist [chunk (cdr content)] (p chunk))]))
      (flush port))))

;; API
;; returns Request
;; If no-response, close connection immediately without sending response.
(define (respond/ng req code :key (keepalive #f) (no-response #f))
  (unless no-response
    (%respond req code "text/plain; charset=utf-8"
              (hash-table-get *status-code-map* code "")))
  (unless (and keepalive (not no-response))
    (socket-close (request-socket req)))
  req)

;; API
;; returns Request
(define (respond/ok req body :key (keepalive #f) (content-type #f))
  (define (resp ctype body) (%respond req 200 (or content-type ctype) body))
  (define has-body? (not (eq? (request-method req) 'HEAD)))
  (unwind-protect
      (guard (e [else (error-log "respond/ok error ~s" (~ e'message))
                      (respond/ng req 500)])
        (match body
          [(? string?) (resp "text/html; charset=utf-8" body)]
          [(? u8vector?) (resp "application/binary" body)]
          [('file filename)
           (let ([ctype (rxmatch-case filename
                          [#/\.js$/ () "application/javascript; charset=utf-8"]
                          [#/\.png$/ () "image/png"]
                          [#/\.(jpg|jpeg)$/ () "image/jpeg"]
                          [#/\.css$/ () "text/css"]
                          [#/\.(mpg|mpeg)$/ () "video/mpeg"]
                          [#/\.wav$/ () "audio/wav"]
                          [#/\.(html|htm)$/ () "text/html; charset=utf-8"]
                          [else "text/plain"])] ;ideally use file magic
                 [content (%fetch-file-content filename has-body?)])
             (if content
               (resp ctype content)
               (respond/ng req 404)))]
          [('plain obj) (resp "text/plain; charset=utf-8"
                              (write-to-string obj display))]
          [('json alist)(resp "application/json; charset=utf-8"
                              (alist->json alist))]
          [('sxml node) (resp "text/html; charset=utf-8"
                              (tree->string (sxml:sxml->html node)))]
          [('chunks . chunks)
           ;; NB: Once we support chunked output, we don't need to calculate
           ;; the total length.
           (resp "application/octet-stream" ; take safe side for the default
                 `(,(fold (^[c s]
                            (+ s (cond [(string? c) (string-size c)]
                                       [(u8vector? c) (uvector-size c)]
                                       [else (error "invalid chunk:" c)])))
                          0 chunks)
                   ,@chunks))]
          [((? symbol? y) .  _) (error "invalid response body type:" y)]
          [else (resp "text/html; charset=utf-8" (tree->string body))])
        req)
    (unless keepalive (socket-close (request-socket req)))))

;; NB: We can't use rfc.json due to the bug exists until Gauche-0.9.2.
;; After releasing 0.9.3, discard these and replace the call of alist->json
;; to compose-json-string.
(define (alist->json alist)
  (tree->string
   `("{",($ intersperse ","
            $ map (^p `(,(write-to-string (x->string (car p)))
                        ":",(item->json (cdr p))))
                  alist)
     "}")))

(define (item->json item)
  (match item
    [#(elt ...)    `("[" ,@(intersperse "," (map item->json elt)) "]")]
    [((x . y) . _) (alist->json item)]
    [_ (write-to-string item)]))

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
;; The server test the request path against path-rx and calls matching handler.
;; A handler can be defined declaratively:
;;
;;   (define-http-handler path-rx handler)
;;
;; Or can be registered procedurally:
;;
;;   (add-http-handler! path-rx handler)

;; API
(define-syntax define-http-handler
  (syntax-rules ()
    [(_ path-rx handler) (add-http-handler! path-rx handler)]))

;; API
(define (add-http-handler! path-rx handler)
  (enqueue! *handlers* (cons path-rx handler)))

;; returns (handler rxmatch)
(define (find-handler path)
  (any-in-queue (^p (and-let* ([m ((car p) path)])
                      `(,(cdr p) ,m)))
                *handlers*))

;;;
;;; Main loop
;;;

;; API
(define (start-http-server :key (host #f)
                                (port 8080)
                                (document-root ".")
                                (num-threads 5)
                                (max-backlog 10)
                                ((:access-log alog) #f)
                                ((:error-log elog) #f)
                                (forwarded? #f)
                                (app-data #f))
  ;; see initial-log-drain for the possible values of access-log and error-log.
  (parameterize ([access-log-drain (initial-log-drain alog 'access-log)]
                 [error-log-drain (initial-log-drain elog 'error-log)]
                 [docroot document-root])
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
            (access-log "~a: started on ~a" (logtime (current-time))
                        (map (.$ sockaddr-name socket-address) ssocks))
            (while #t (selector-select sel)))
        (access-log "~a: terminating" (logtime (current-time)))
        (for-each socket-close ssocks)
        (tpool:terminate-all! pool :force-timeout 300)
        (thread-terminate! tlog)))))

(define (kick-logger-thread pool forwarded?)
  (thread-start! (make-thread (cut logger pool forwarded?))))

(define (accept-client app csock pool)
  (unless (tpool:add-job! pool (cut handle-client app csock) #t)
    (respond/ng (make-ng-request "[E] too many request backlog" csock) 503)
    (socket-close csock)))

(define (handle-client app csock)
  (guard (e [else
             (error-log "handle-client error ~s" (~ e'message))
             (respond/ng (make-ng-request #`"[E] ,(~ e'message)" csock) 500)])
    (let* ([iport (socket-input-port csock)]
           [line (read-line (socket-input-port csock))])
      (rxmatch-case line
        [test eof-object?
         (respond/ng (make-ng-request "(empty request)" csock) 400
                     :no-response #t)]
        [#/^(GET|HEAD|POST)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
         (receive (auth path query frag) (uri-decompose-hierarchical abs-path)
           (let* ([path (uri-decode-string path :cgi-decode #t)]
                  [handler&match (find-handler path)]
                  [hdrs (rfc822-read-headers iport)]
                  [host ($ rfc822-header-ref hdrs "host"
                           $ sockaddr-name $ socket-getsockname csock)]
                  [req (make-request line csock meth host path
                                     (cond [handler&match => cadr] [else #f])
                                     query hdrs)])
             (if handler&match
               ((car handler&match) req app)
               (respond/ng req 404))))]
        [#/^[A-Z]+.*/ ()
          (respond/ng (make-ng-request #`"[E] ,line" csock) 501)]
        [else (respond/ng (make-ng-request #`"[E] ,line" csock) 400)]))))

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

(define (logger pool forwarded?)
  (guard (e [else (error-log "[I] logger error: ~a" (~ e'message))])
    (let loop ()
      (let1 j (dequeue/wait! (~ pool'result-queue))
        (case (job-status j)
          [(done) (let1 r (job-result j)
                    (unless (request? r)
                      (error "some handler didn't return request:" r))
                    (access-log "~a: ~a \"~a\" ~a ~a ~s ~a"
                                (logtime (job-acknowledge-time j))
                                (or (and forwarded?
                                         (request-header-ref r "x-forwarded-for"))
                                    (logip (request-remote-addr r)))
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
(define (cgi-handler proc :key (script-name ""))
  ;; NB: We should be able to get away from concatenating entire output---
  ;; for future extension.
  (define (uvector-concatenate uvs)
    (let1 dest (make-u8vector (fold (^[v s] (+ (u8vector-length v) s)) 0 uvs))
      (let loop ([pos 0] [uvs uvs])
        (cond [(null? uvs) dest]
              [else (u8vector-copy! dest pos (car uvs))
                    (loop (+ pos (u8vector-length (car uvs))) (cdr uvs))]))))
  (define (header+content vec)
    (let* ([p (open-input-uvector vec)]
           [hdrs (rfc822-read-headers p)]
           [pos (port-tell p)])
      (values hdrs
              (uvector-alias <u8vector> vec pos)
              (- (u8vector-length vec) pos))))
  (^[req app]
    (let1 q (make-queue)
      (with-input-from-port (request-iport req)
        (^[]
          (let1 r (parameterize ([cgi-metavariables
                                  (get-cgi-metavariables req script-name)]
                                 [current-output-port
                                  (make <buffered-output-port>
                                    :flush (^[v f] (enqueue! q v)
                                                   (u8vector-length v)))])
                    (unwind-protect (proc "")
                      (close-output-port (current-output-port))))
            (if (zero? r)
              (receive (hdrs content content-length)
                  (header+content (uvector-concatenate (dequeue-all! q)))
                (dolist [h hdrs] (response-header-push! req (car h) (cadr h)))
                (respond/ok req content
                            :content-type
                            (rfc822-header-ref hdrs "content-type")))
              (respond/ng req 500))))))))

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
   [#t `("PATH_INFO" ,(request-path req))]
   [#t `("PATH_TRANSLATED" ;todo - flexible path trans.
         ,(string-append (docroot) (request-path req)))]
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

;;;
;;; Adds header
;;;

(define (with-header-handler inner-handler . header&values)
  (^[req app]
    (dolist [h&v (slices header&values 2 :fill? #t)]
      (if-let1 val (if (or (string? (cadr h&v)) (not (cadr h&v)))
                     (cadr h&v)
                     ((cadr h&v) req app))
        (response-header-push! req (x->string (car h&v)) val)))
    (inner-handler req app)))

