;;;
;;; makiki - a small http server
;;;
;;;   Copyright (c) 2010-2018 Shiro Kawai  <shiro@acm.org>
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
;;;

(define-module makiki
  (use control.job)
  (use control.thread-pool :prefix tpool:)
  (use data.queue)
  (use file.util)
  (use gauche.collection)
  (use gauche.generator)
  (use gauche.logger)
  (use gauche.net)
  (use gauche.record)
  (use gauche.selector)
  (use gauche.sequence)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.version)
  (use gauche.vport)
  (use rfc.822)
  (use rfc.cookie)
  (use rfc.http)
  (use rfc.mime)
  (use rfc.uri)
  (use rfc.tls)
  (use srfi.13)
  (use srfi.19)
  (use sxml.tools)
  (use text.html-lite)
  (use text.tree)
  (use util.match)
  (use www.cgi)
  (export start-http-server http-server-software
          access-log access-log-drain
          error-log error-log-drain
          debugging auto-secure-cookie
          add-method-dispatcher!
          make-server-control-channel
          terminate-server-loop
          request? request-socket request-iport request-oport
          request-remote-addr request-query
          request-line request-method request-uri request-http-version
          request-server-host request-server-port
          request-path request-path-match request-path-ref request-path-rxmatch
          request-guard-value
          request-response-error
          request-params  request-param-ref
          request-headers request-header-ref
          request-cookies request-cookie-ref
          <request-error> request-error
          respond/ng respond/ok respond/redirect
          respond-callback-add!
          response-header-push! response-header-delete!
          response-header-replace!
          response-cookie-add! response-cookie-delete!
          read-request-body
          let-params
          define-http-handler add-http-handler!
          path:int path:hex path:symbol
          document-root
          file-handler file-mime-type
          with-header-handler with-post-parameters with-post-json
          with-profiling-handler
          profiler-output)
  )
(select-module makiki)

(autoload rfc.json construct-json-string parse-json-string <json-parse-error>)

(autoload rfc.uuid parse-uuid)

;;;
;;; Some parameters
;;;

(define document-root (make-parameter "."))
(define http-server-software (make-parameter "gauche/makiki"))
(define file-mime-type (make-parameter (^[path] #f)))
(define profiler-output (make-parameter (sys-getenv "MAKIKI_PROFILER_OUTPUT")))
(define debugging (make-parameter (sys-getenv "MAKIKI_DEBUGGING")))
(define auto-secure-cookie (make-parameter #t))

;; This is for development.  An implicit side-channel to stop the server.
;; Not for general use.
(define dev-control-channel (make-parameter #f))

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
  secure              ; #f: plain, #t: ssl, forwarded: https rev proxy
  (path)              ; request path (string)
  (path-match)        ; proc to extract matched path component (set by dispatcher)
  (guard-value)       ; the result of guard procedure (set by dispatcher)
  (query)             ; query string as passed in the request
  (params)            ; parsed query parameters
  (headers)           ; request headers
  (response-error)    ; #f if response successfully sent, #<error> otherwise.
                      ;  set by respond/* procedures.
  ;; private slots
  (cookies %request-cookies request-cookies-set!) ; promise of alist of parsed cookies
  send-cookies        ; hashtable {cookie-name::<string> -> (name value . opts)}
  (status)            ; result status (set by responder)
  (response-headers)  ; response headers (set by handler)
  (response-size))    ; size of reply content in octets (set by responder)

(define-inline (make-request request-line csock method request-uri
                             http-vers headers)
  (define host:port ($ rfc822-header-ref headers "host"
                       $ sockaddr-name $ connection-self-address csock))
  (define-values (auth path query frag)
    (uri-decompose-hierarchical request-uri))
  (define-values (host port)
    (if-let1 m (#/:(\d+)$/ host:port)
      (values (m'before) (x->integer (m 1)))
      (values host:port 80)))
  (define secure-channel?               ; a kind of heuristics
    (cond [(is-a? csock <tls>) #t]
          [(equal? (rfc822-header-ref headers "x-forwarded-proto") "https")
           'forwarded]
          [else #f]))
  (%make-request request-line csock (connection-peer-address csock)
                 method request-uri http-vers host port secure-channel?
                 (uri-decode-string path :cgi-decode #t) #f #f
                 query (cgi-parse-parameters :query-string (or query ""))
                 headers #f (delay (%request-parse-cookies headers))
                 (make-hash-table 'string=?)
                 #f '() 0))

(define-inline (make-ng-request msg socket)
  (%make-request msg socket (connection-peer-address socket) ""
                 "" "" "" 80 "" #f #f "" '() '() #f '() '() #f '() 0))

;; Backward compatibility
(define (request-path-rxmatch req)
  (rlet1 r (request-path-match req)
    (unless (or (not r) (is-a? r <regmatch>))
      (error "Attempt to use request-path-rxmatch on a request not using \
             regexp path match" req))))

;; API
(define (request-path-ref req key :optional (default #f))
  (if-let1 m (request-path-match req)
    (m key)
    default))

;; API
;; The handler can throw this condition to respond to the client
(define-condition-type <request-error> <error> #f
  (status) (body) (content-type))
(define (request-error :key (status 400) (body #f) (content-type #f))
  (error <request-error> :status status :body body :content-type content-type
         (x->string body)))

;; APIs
(define-inline (request-iport req) (connection-input-port (request-socket req)))
(define-inline (request-oport req) (connection-output-port (request-socket req)))

;; API
;; Read request body into u8vector.  May return #f if request has no body.
;; May return #<eof> if the body is already (partially) read.
(define (read-request-body req)
  (and-let* ([slen (request-header-ref req "content-length")]
             [len (x->integer slen)]
             [buf (make-u8vector len)])
    (let loop ([nread 0])
      (if (= nread len)
        buf
        (let1 n (read-uvector! buf (request-iport req) nread len)
          (if (eof-object? n)
            (eof-object)
            (loop (+ nread n))))))))

;; some convenience accessors
;; APIs
(define (request-param-ref req param-name . keys)
  (apply cgi-get-parameter param-name (request-params req) keys))
(define (request-header-ref req header-name :optional (default #f))
  (rfc822-header-ref (request-headers req) header-name default))
(define (response-header-push! req header-name value)
  (push! (request-response-headers req) (list header-name value)))
(define (response-header-delete! req header-name)
  (update! (request-response-headers req)
           (cut remove (^e (equal? (car e) header-name)) <>)))
(define (response-header-replace! req header-name value)
  (response-header-delete! req header-name)
  (response-header-push! req header-name value))

;; Cookie utilities
(define (%request-parse-cookies headers)
  (parse-cookie-string (or (rfc822-header-ref headers "cookie")
                           (rfc822-header-ref headers "cookie2")
                           "")))

;; APIs
(define (request-cookies req) (force (%request-cookies req)))
(define (request-cookie-ref req name :optional (default #f))
  (or (assoc name (request-cookies req)) default))
(define (response-cookie-add! req name value . options)
  (hash-table-put! (request-send-cookies req) name
                   `(,name ,value ,@options)))
(define (response-cookie-delete! req name)
  (hash-table-put! (request-send-cookies req) name
                   `(,name "" :expires 0)))

;; API
;; (let-params req ((var spec ...) ...) body ...)
;; spec : [source] kv-list ...
;; source is a string with the format "kind" or "kind:name".
;; kind part is one-character indicating where the value is taken:
;;   "q" - Query params (default)
;;   "p" - path-regexp match
;;   "c" - cookie
;;   "h" - header
;; The optional name part specifies the name of the value; when
;; omitted, the variable name is used.
;; The following keys are recognized in kv-list:
;;   :default   - default value.  if omitted, #f.
;;   :convert   - proc applied to the string
;;   :list      - if true, multiple value is allowed.
;;
(define-macro (let-params req bindings . body)
  (define tmp-req (gensym))
  (define tmp-unique (gensym))
  (define (extractor var-spec)
    (match var-spec
      [((? symbol? var)) (kv param-extractor var #"~var" '())]
      [(? symbol? var)   (kv param-extractor var #"~var" '())]
      [((? symbol? var) (? string? spec) . kv-list)
       (rxmatch-case spec
         [#/^([qpch])(?::(.+))?$/ (_ kind name)
          (let1 name (or name #"~var")
            (cond [(equal? kind "q") (kv param-extractor var name kv-list)]
                  [(equal? kind "p") (kv path-extractor var name kv-list)]
                  [(equal? kind "c") (kv cookie-extractor var name kv-list)]
                  [(equal? kind "h") (kv header-extractor var name kv-list)]))]
         [else (error "Bad source spec:" spec)])]
      [_ (error "Invalid var-spec:" var-spec)]))
  (define (kv proc var name args)
    (let-keywords* args ([default #f] [convert #f] [list #f])
      (proc var name default convert list)))
  (define (param-extractor var name default cv lis?)
    `[,var (request-param-ref ,tmp-req ,name
                              ,@(cond-list [default @ `(:default ,default)]
                                           [cv @ `(:convert ,cv)]
                                           [lis? @ `(:list ,lis?)]))])
  (define (path-extractor var name default cv lis?)
    (let ([matchname (if (#/^\d+$/ name)
                       (x->integer name)
                       `',(string->symbol name))]
          [tmp (gensym)])
      `[,var
        ,(if cv
           `(if-let1 ,tmp
                ((request-path-match ,tmp-req) ,matchname)
              (,cv ,tmp)
              ,default)
           `(or ((request-path-match ,tmp-req) ,matchname)
                ,default))]))
  (define (header-extractor var name default cv lis?)
    (let1 tmp (gensym)
      `[,var
        ,(if cv
           `(let1 ,tmp (request-header-ref ,tmp-req ,name ,tmp-unique)
             (if (eq? ,tmp ,tmp-unique)
               ,default
               (,cv ,tmp)))
           `(request-header-ref ,tmp-req ,name ,default))]))
  (define (cookie-extractor var name default cv lis?)
    (let1 tmp (gensym)
      `[,var
        (if-let1 ,tmp (request-cookie-ref ,tmp-req ,name)
          ,(if cv `(,cv (cadr ,tmp)) `(cadr ,tmp))
          ,default)]))
  `(let ([,tmp-req ,req]
         [,tmp-unique (cons #f #f)])
     (let ,(map extractor bindings)
       ,@body)))

;;;
;;; Generating response
;;;

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
  (dolist [cb (reverse (%respond-callbacks))]
    (cb req code content-type))
  (let ([port (request-oport req)]
        [desc (or (http-status-code->description code) "")])
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
      ($ %prepare-response-cookies req
         $ hash-table-values (request-send-cookies req))
      (dolist [h (request-response-headers req)]
        (dolist [v (cdr h)]
          (p (car h)) (p ": ") (p v) (crlf)))
      (crlf)
      (unless (eq? (request-method req) 'HEAD)
        (cond [(or (string? content) (u8vector? content)) (p content)]
              [(pair? content) (dolist [chunk (cdr content)] (p chunk))]))
      (flush port))))

(define (%prepare-response-cookies req cookies)
  (dolist [cookie cookies]
    (match-let1 (name value . opts) cookie
      (let* ([opts (if (and (~ req'secure) (auto-secure-cookie))
                     `(:secure #t ,@(delete-keyword :secure opts))
                     opts)]
             ;; TRANSIENT: Workaround of rfc.cookie 'Expires' header
             ;; is regarded as obsoleted in the old RFC (it was in the
             ;; original Netscape spec).  However, RFC6265
             ;; reintroduced it.  Until Gauche is updated, we need the
             ;; explicit version for construct-cookie-string to make
             ;; it recognize Expires attribute.
             [vers (if (get-keyword :expires opts #f) 0 1)])
        (dolist [cs (construct-cookie-string `((,name ,value ,@opts)) vers)]
          (response-header-push! req "set-cookie" cs))))))

;; API
;;   Respond callback is invoked before response is sent to the client.
;;   You can add cookies and/or headers here.  You can't modify response code
;;   or response body.
;;   proc := (<^> <req> code::<integer> content-type::<string>)
(define (respond-callback-add! proc)
  (push! (%respond-callbacks) proc))

(define %respond-callbacks (make-parameter '()))

;; Handle 'body' argument for respond/ok and respond/ng.  Returns
;; content-type and <content>.  If content-type arg is #f, we assume
;; the default content type, according to the BODY form.  Note that
;; 'file' body may raise an condition 404 if the file doesn't exist.
;;
;; Supported forms:
;;  <string>
;;  <u8vector>
;;  (file <filename>)
;;  (plain <object>)
;;  (json <alist-or-vector>)
;;  (sxml <sxml> . <subst-alist>)
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
           [('html . _)
            (v "text/html; charset=utf-8"
               (tree->string `("<!DOCTYPE html>\n" ,(sxml:sxml->html n))))]
           [_
            (v "application/xml"
               (tree->string `("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                               ,(sxml:sxml->xml n))))]))]
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
               (or body (http-status-code->description code) "") #f)
          (%respond req code content-type content)))
      (%respond req code "text/plain; charset=utf-8"
                (or (http-status-code->description code) ""))))
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

;; We use a queue simply to add new handler at the end of list, so that
;; the one defined earlier takes precedence.  NB: This may be changed
;; in future.
(define *handlers* (make-mtqueue))

;; This is for The dev-time convenience.  Clear the handler list before
;; reloading the script.
(define (clear-handlers!) (set! *handlers* (make-mtqueue)))

;; The server program registers appropriate handlers.
;;
;;  handler :: Request App -> IO ()
;;
;; The handler is responsible to process the request, and to call respond/*
;; procedure to send the response.
;;
;; Handlers can be registered by define-http-handler:
;;
;;   (define-http-handler [(method ...)] pattern [? guard] handler)
;;
;; METHOD is a symbol (GET, POST, etc.) that this handler accepts.  If omitted,
;; (GET HEAD POST) is assumed.
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
    [(_ (m ...) pattern ? guard handler)
     (add-http-handler! (handle-pattern pattern) handler guard '(m ...))]
    [(_ (m ...) pattern handler)
     (add-http-handler! (handle-pattern pattern) handler #f '(m ...))]
    [(_ pattern ? guard handler)
     (add-http-handler! (handle-pattern pattern) handler guard #f)]
    [(_ pattern handler)
     (add-http-handler! (handle-pattern pattern) handler #f #f)]))

(define-syntax handle-pattern
  (er-macro-transformer
   (^[f r c]
     (let1 pattern (cadr f)
       (cond [(string? pattern) pattern]
             [(regexp? pattern) pattern]
             [(pair? pattern)
              `(,(r'list*)
                ,@(map* (^[pat]
                          (match pat
                            [(? string?) pat]
                            [(? identifier?) `(,(r'quote) ,pat)]
                            [(expr var ...) (=> fail)
                             (unless (every identifier? var) (fail))
                             `(,(r'list) ,expr
                               ,@(map (^v `(,(r'quote) ,v)) var))]
                            [else
                             (error "Invalid pattern element:" pat)]))
                        (^[tail]
                          (unless (or (null? tail) (identifier? tail))
                            (error "Invalid pattern:" pattern))
                          `((,(r'quote) ,tail)))
                        pattern))]
             [else (error "Invalid pattern:" pattern)])))))

;; Matcher :: (path, [commponents]) -> matchobj | #f
;; matchobj :: key -> matchval
(define (build-matcher pattern)
  (cond [(string? pattern)
         (^[path components]
           (and (string=? path pattern) (constantly #t)))]
        [(regexp? pattern)
         (^[path components] (pattern path))]
        [(or (null? pattern) (pair? pattern))
         (^[path components]
           (let loop ([pats pattern]
                      [comps components]
                      [matched '()])
             (match pats
               [() (and (null? comps)
                        (make-matched matched))]
               [(pat . pats)
                (and (pair? comps)
                     (cond [(string? pat)
                            (and (equal? pat (car comps))
                                 (loop pats (cdr comps) matched))]
                           [(symbol? pat)
                            (loop pats (cdr comps)
                                  (acons pat (car comps) matched))]
                           [(list? pat)
                            (and-let1 m (component-match pat (car comps) matched)
                              (loop pats (cdr comps) m))]
                           [else
                            (error "Invalid element in pattern:" pat)]))]
               [rest-pat
                (make-matched (acons rest-pat
                                     (string-join comps "/")
                                     matched))])))]
        [else
         (error "Bad pattern:" pattern)]))

;; Handle (expr var ...) component in PATTERN.
(define (component-match pattern component alist)
  (match-let1 [expr . vars] pattern
    (if (regexp? expr)
      (and-let1 m (expr component)
        (fold-with-index (^[index var alist] (acons var (m index) alist))
                         alist vars))
      (receive rs (expr component)
        (match rs
          [(#f . _) #f]
          [else (fold acons alist vars rs)])))))

;; Returns a lookup procedure suitable for request-path-match.
(define (make-matched alist)
  (^[key] (assq-ref alist key)))

;; API - utility to be used for path component matching
(define (path:int comp)
  (and-let* ([n (string->number comp)]
             [ (exact-integer? n) ])
    n))
(define (path:hex comp)
  (and-let* ([n (string->number comp 16)]
             [ (exact-integer? n) ])
    n))
(define ((path:symbol rx) comp)
  (and-let1 m (rx comp)
    (string->symbol (m))))

(cond-expand
 [(library rfc.uuid)
  ;; After 0.9.13, we can use (parse-uuid comp :if-invalid #f), but
  ;; for now we have to trap uuid parse error.
  (export path:uuid)
  (define (path:uuid comp)
    (guard (e [else #f]) (parse-uuid comp)))]
 [else])

;; API
(define (add-http-handler! pattern handler :optional (guard #f) (methods #f))
  (let ([guard (or guard (^[m a] #t))]
        [methods (or methods '(GET HEAD POST))]
        [matcher (build-matcher pattern)])
    (enqueue! *handlers* (list methods matcher guard handler))))

;; returns (handler req)
(define (find-handler path req app)
  (let ([method (request-method req)]
        [path-components (string-split (string-trim-right path #[/])
                                       #[/] 'prefix)])
    (any-in-queue (^[entry]
                    (match-let1 (methods matcher guard handler) entry
                      (and-let* ([ (memq method methods) ]
                                 [m (matcher path path-components)]
                                 [g (begin (request-path-match-set! req m)
                                           (guard req app))])
                        (request-guard-value-set! req g)
                        (list handler req))))
                  *handlers*)))

;; API
;; Create a server control channel, through which you can terminate
;; the server.  The implementation detail may change; the user must treat
;; this as an opaque object.
(define (make-server-control-channel)
  (receive (t-in t-out) (sys-pipe)
    (define value #f)
    (^[msg . args]
      (ecase msg
        [(get-channel) t-in]
        [(get-value) value]
        [(request-termination) (set! value (car args)) (newline t-out)]))))

;; API
;; Request termination of the server loop
(define (terminate-server-loop control-channel exit-value)
  (control-channel 'request-termination exit-value))

;;;
;;; Server loop
;;;

;; API
;; This procedure won't return until the server shuts down.
(define (start-http-server :key (host #f)
                                (port #f)
                                (tls-port #f)
                                (path #f) ; unix domain
                                ((:document-root docroot) ".")
                                ((:auto-secure-cookie scookie) #t)
                                (num-threads 5)
                                (max-backlog 10)
                                ((:access-log alog) #f)
                                ((:error-log elog) #f)
                                (tls-settings '())
                                (forwarded? #f)
                                (app-data #f)
                                (control-channel #f)
                                (startup-callback #f)
                                (shutdown-callback #f))
  ;; see initial-log-drain for the possible values of access-log and error-log.
  (parameterize ([access-log-drain (initial-log-drain alog 'access-log)]
                 [error-log-drain (initial-log-drain elog 'error-log)]
                 [document-root docroot]
                 [auto-secure-cookie scookie])
    (let* ([pool (tpool:make-thread-pool num-threads :max-backlog max-backlog)]
           [tlog (kick-logger-thread pool forwarded?)]
           ;; If none of port, tls-port and path is given, use port 8080
           ;; for the convenience.
           [default-port (or port (and (not tls-port) (not path) 8080))]
           [ssocks (cond-list
                    [path (list (make-server-socket 'unix path))]
                    [default-port
                     @ (make-server-sockets host default-port :reuse-addr? #t)]
                    [tls-port
                     (make-tls-sockets host tls-port tls-settings)])])
      (guard (e [(and (<unhandled-signal-error> e)
                      (memv (~ e'signal) `(,SIGINT ,SIGTERM)))
                 (access-log "Shutdown by signal ~a" (~ e'signal))]
                [else (raise e)])
        (unwind-protect
            (server-loop ssocks app-data pool startup-callback control-channel)
          (access-log "Server terminating...")
          (for-each %socket-discard ssocks)
          (tpool:terminate-all! pool :force-timeout 300)
          (thread-terminate! tlog)
          (when path (sys-unlink path)) ; remove unix socket
          (when shutdown-callback (shutdown-callback)))))))

;; Main loop.  We can't use <selector> for TLS socket (it doesn't expose
;; underlying fd), so we use a kludge---we run a thread for each TLS sock
;; to poll, and use a pipe to tell the incoming request to the main loop.
(define (server-loop server-sockets app-data pool
                     startup-callback control-channel)
  (define looping (box #t))
  (define exit-value 0)
  (define sel (make <selector>))
  (dolist [s server-sockets]
    (if (is-a? s <socket>)
      ($ selector-add! sel (socket-fd s)
         (^[fd condition]
           (accept-client app-data (socket-accept s) pool))
         '(r))
      (let1 p (kick-tls-poll-thread s looping)
        ($ selector-add! sel (port-file-number p)
           (^[fd condition]
             (read-byte p) ; discard dummy byte
             (accept-client app-data (tls-accept s) pool))
           '(r)))))
  (dolist [cch (list control-channel (dev-control-channel))]
    (when cch
      (selector-add! sel (cch 'get-channel)
                     (^[fd condition]
                       (set-box! looping #f)
                       (set! exit-value (cch 'get-value)))
                     '(r))))
  (when startup-callback (startup-callback server-sockets))
  ($ access-log "Started on ~a"
     (map (.$ connection-address-name connection-self-address) server-sockets))
  ;; Main loop
  (while (unbox looping) (selector-select sel))
  exit-value)

(define (make-tls-sockets host port tls-settings)
  (unless (version>=? (gauche-version) "0.9.14")
    (error "TLS support is not enough in this Gauche.  Use newer version."))
  (let ([certificates (get-keyword :tls-certificates tls-settings '())]
        [private-key  (get-keyword :tls-private-key tls-settings #f)]
        [password     (get-keyword :tls-private-key-password tls-settings #f)]
        [tls (make-tls)])
    (for-each (cut tls-load-certificate tls <>) certificates)
    (when private-key
      (tls-load-private-key tls private-key password))
    (tls-bind tls host port)
    tls))

;; Returns an input fd to monitor
(define (kick-tls-poll-thread tls looping)
  (define-values (in out) (sys-pipe))
  ;; for graceful shutdown, we periodically check if stop is requested
  (define (poll-thunk)
    (let loop ()
      (if (null? (tls-poll tls '(read) 3.0))
        ;; timeout
        (when (unbox looping)
          (loop))
        (begin (write-byte 0 out) (loop)))))
  (thread-start! (make-thread poll-thunk))
  in)

(define (kick-logger-thread pool forwarded?)
  (thread-start! (make-thread (cut logger pool forwarded?))))

(define (%socket-discard sock)
  (connection-close sock)
  (connection-shutdown sock 'both))

(define (accept-client app csock pool)
  (unless (tpool:add-job! pool (cut handle-client app csock) #t)
    (respond/ng (make-ng-request "[E] too many request backlog" csock) 503)
    (%socket-discard csock)))

(define (handle-client app csock)
  (guard (e [else
             (error-log "handle-client error ~s\n~a" (~ e'message)
                        (report-error e #f))
             (respond/ng (make-ng-request #"[E] ~(~ e'message)" csock) 500)])
    (let1 line (read-line (connection-input-port csock))
      (rxmatch-case line
        [test eof-object?
         (respond/ng (make-ng-request "(empty request)" csock) 400
                     :no-response #t)]
        [#/^(\w+)\s+(\S+)\s+HTTP\/(\d+\.\d+)$/ (_ meth req-uri httpvers)
         (let* ([method (string->symbol (string-upcase meth))]
                [headers (rfc822-read-headers (connection-input-port csock))]
                [req (make-request line csock method req-uri httpvers headers)])
           (parameterize ([%respond-callbacks '()])
             (if-let1 dispatcher (find-method-dispatcher method)
               (guard (e [(<request-error> e)
                          (respond/ng req (~ e'status) :body (~ e'body)
                                      :content-type (~ e'content-type))]
                         [else
                          (error-log "http handler error ~s\n~a" (~ e'message)
                                     (report-error e #f))
                          (receive (status content-type body)
                              (server-error-handler req app e)
                            (respond/ng req status :body body
                                        :content-type content-type))])
                 (dispatch-worker dispatcher req app))
               (respond/ng (make-ng-request #"[E] ~line" csock) 501))))]
        [else (respond/ng (make-ng-request #"[E] ~line" csock) 400)]))))

(define (server-error-handler req app e) ;returns status, content-type and body
  (define (parse-accept-header value)
    (map (^s (match-let1 (content-type . opts) (string-split s #/\s*\;\s*/)
               (cons content-type
                     (map (^p (string-split (string-trim-both p) "=" 1)) opts))))
         (string-split value #/\s*,\s*/)))
  (define (select-reply-content-type accepts-alist)
    ;; We should look at q value, but it's unlikely that client wants both
    ;; json and html.
    (find (^[content-type] (and (assoc-ref accepts-alist content-type)
                                content-type))
          '("application/json" "text/html" "text/plain")))
  (define ISE "Internal Server Error")
  (define (html-error-page)
    (html:html
     (html:head (html:title ISE))
     (html:body (html:h1 ISE)
                (html:p (if (debugging) (~ e'message) "")))))
  (define (text-error-page)
    (if (debugging) #"~|ISE|: ~(~ e'message)" ISE))
  (define (json-error-page)
    `(json (("status" . 500)
            ("message" . ,(if (debugging) #"~|ISE|: ~(~ e'message)" ISE)))))
  (let* ([content-type ($ select-reply-content-type $ parse-accept-header
                          $ request-header-ref req "accept" "")]
         [body (cond
                [(equal? content-type "application/json") (json-error-page)]
                [(equal? content-type "text/html") (html-error-page)]
                [else (text-error-page)])])
    (values 500 content-type body)))

(define (dispatch-worker dispatcher req app)
  (if-let1 prof-out (profiler-output)
    (with-profiling-handler prof-out dispatcher)
    (dispatcher req app)))

(define *method-dispatchers* (make-mtqueue))

(define (find-method-dispatcher method)
  (any-in-queue (^p (and (eq? (car p) method) (cdr p))) *method-dispatchers*))

;; API
(define (add-method-dispatcher! meth proc)
  (enqueue! *method-dispatchers* (cons meth proc)))

;; Default dispatcher
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

(dolist [m '(GET HEAD POST PUT PATCH DELETE OPTIONS)]
  (add-method-dispatcher! m %default-dispatch))

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

(define (logip addr) ; NB: probably this should be a feature in gauche.net!
  (case (sockaddr-family addr)
    [(unix) "unix-socket"]
    [(inet)  (inet-address->string (sockaddr-addr addr) AF_INET)]
    [(inet6) (inet-address->string (sockaddr-addr addr) AF_INET6)]
    [else (x->string addr)]))

(define (logdt t0 t1)
  (let1 dt (time-difference t1 t0)
    (format "~:d.~3,'0dms"
            (+ (* (time-second dt) 1000)
               (quotient (time-nanosecond dt) 1000000))
            (modulo (quotient (time-nanosecond dt) 1000) 1000))))

(define (logreferer req)
  (rfc822-header-ref (request-headers req) "referer" "-"))

;;;
;;; Built-in file handler
;;;

;; API
(define (file-handler :key (directory-index '("index.html" #t))
                           (path-trans request-path)
                           (root #f))
  (^[req app] (%handle-file req directory-index root path-trans)))

(define (%handle-file req dirindex root path-trans)
  (let ([root (or root (document-root))]
        [rpath (sys-normalize-pathname (path-trans req) :canonicalize #t)])
    (if (or (string-prefix? "/../" rpath)
            (string=? "/.." rpath))
      (respond/ng req 403)      ;do not allow path traversal
      (let1 fpath (sys-normalize-pathname #"~|root|~rpath")
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
;;; Handler wrappers
;;;

;; API
;; Add headers.
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

;; API
;; Read POST body as application/x-www-form-urlencoded or multipart/form-data
;; and fold result into request-params.
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

;; API
;; Read POST body as JSON and set result in the "json-body" parameter in
;; request-params.
;; on-error is invoked with req, app and <json-parse-error> in case of
;; json parse error.
(define (with-post-json inner-handler :key (on-error #f))
  (^[req app]
    (if (eq? (request-method req) 'POST)
      (let* ([body (read-request-body req)]
             [json (and body
                        (u8vector? body)
                        (guard (e [(<json-parse-error> e)
                                   (if on-error
                                     (on-error req app e)
                                     (request-error
                                      :status 400
                                      :body #"invalid json: ~(~ e'message)"))])
                          (parse-json-string (u8vector->string body))))])
        (request-params-set! req `(("json-body" ,json) ,@(request-params req)))
        (inner-handler req app))
      (inner-handler req app))))

;; API
;; Wrap handler with profiler.  The profiler result is written out to
;; the file named by PROF-OUT.
(define with-profiling-handler
  (let1 mutex (make-mutex)
    (^[prof-out handler]
      (^[req app]
        (let1 out (open-output-string)
          (begin0 ($ with-output-to-port out
                     (cut with-profiler (cut handler req app)))
            (with-locking-mutex mutex
              (^[] (with-output-to-file prof-out
                     (^[]
                       (format #t "~a: ~a\n"
                               (logtime (current-time))
                               (request-path req))
                       (display (get-output-string out))
                       (newline))
                     :if-exists :append)))))))))
