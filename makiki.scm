;;;
;;;   Copyright (c) 2010 Shiro Kawai  <shiro@acm.org>
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
  (use text.html-lite)
  (use util.list)
  (use util.queue)
  (use util.match)
  (use www.cgi)
  (export start-http-server httpd-log-drain
          request?
          request-socket request-iport request-oport
          request-path request-params request-headers
          respond/ng respond/ok
          define-http-handler
          file-handler)
  )
(select-module makiki)

;;;
;;; Some parameters
;;;

(define docroot (make-parameter "."))

;;;
;;; Logging
;;;

(define httpd-log-drain (make-parameter #f))

(define-macro (log fmt . args)
  (let1 drain (gensym)
    `(if-let1 ,drain (httpd-log-drain)
       (log-format ,drain ,fmt ,@args))))

;;;
;;; Request packet
;;;

(define-record-type request  %make-request #t
  line                               ; the first line of the request
  socket                             ; client socket
  remote-addr                        ; remote address (sockaddr)
  method                             ; request method
  path                               ; request path
  params                             ; query parameters
  headers                            ; request headers
  (status)                           ; result status (set later)
  (reply-size))                      ; size of reply content in octets

(define-inline (make-request line socket method path params headers)
  (%make-request line socket (socket-getpeername socket)
                 method path params headers #f 0))
(define-inline (make-partial-request msg socket)
  (%make-request #`"#<error - ,|msg|>" socket (socket-getpeername socket)
                 "" "" '() '() #f 0))

(define-inline (request-iport req) (socket-input-port (request-socket req)))
(define-inline (request-oport req) (socket-output-port (request-socket req)))

;;;
;;; Generating response
;;;

(define *status-code-map*
  (hash-table 'string=?
              '("100" . "Continue")
              '("101" . "Switching Protocols")
              '("200" . "OK")
              '("201" . "Created")
              '("202" . "Accepted")
              '("203" . "Non-Authoritative Information")
              '("204" . "No Content")
              '("205" . "Reset Content")
              '("206" . "Partial Content")
              '("300" . "Multiple Choices")
              '("301" . "Moved Permanently")
              '("302" . "Found")
              '("303" . "See Other")
              '("304" . "Not Modified")
              '("305" . "Use Proxy")
              '("306" . "(Unused)")
              '("307" . "Temporary Redirect")
              '("400" . "Bad Request")
              '("401" . "Unauthorized")
              '("402" . "Payment Required")
              '("403" . "Forbidden")
              '("404" . "Not Found")
              '("405" . "Method Not Allowed")
              '("406" . "Not Acceptable")
              '("407" . "Proxy Authentication Required")
              '("408" . "Request Timeout")
              '("409" . "Conflict")
              '("410" . "Gone")
              '("411" . "Length Required")
              '("412" . "Precondition Failed")
              '("413" . "Request Entity Too Large")
              '("414" . "Request-URI Too Long")
              '("415" . "Unsupported Media Type")
              '("416" . "Requested Range Not Satisfiable")
              '("417" . "Expectation Failed")
              '("500" . "Internal Server Error")
              '("501" . "Not Implemented")
              '("502" . "Bad Gateway")
              '("503" . "Service Unavailable")
              '("504" . "Gateway Timeout")
              '("505" . "HTTP Version Not Supported")
              ))

(define (%respond code content-type content port)
  (define (p x) (display x port))
  (define (crlf) (display "\r\n" port))
  (let1 desc (hash-table-get *status-code-map* code "")
    (guard (e [(<system-error> e) (log "response error ~s" e)])
      (p "HTTP/1.1 ") (p code) (p " ") (p desc) (crlf)
      (p "Content-Type: ") (p content-type) (crlf)
      (p "Content-Length: ") (p (string-size content)) (crlf)
      (crlf)
      (p content)
      (flush port))))

;; returns Request
(define (respond/ng req code :optional (keepalive #f))
  (%respond code "text/plain; charset=utf-8"
            (hash-table-get *status-code-map* code "")
            (request-oport req))
  (request-status-set! req code)
  (unless keepalive (socket-close (request-socket req)))
  req)

;; returns Request
(define (respond/ok req body :optional (keepalive #f))
  (unwind-protect
      (guard (e [else (log "respond/ok error ~s" (~ e'message))
                      (respond/ng req "500")])
        (let1 oport (request-oport req)
          (match body
            [(? string?)
             (%respond "200" "text/html; charset=utf-8" body oport)]
            [('file filename)
             (let1 ctype (rxmatch-case filename
                           [#/\.js$/ () "application/javascript; charset=uft-8"]
                           [#/\.png$/ () "image/png"]
                           [#/\.jpg$/ () "image/jpeg"]
                           [#/\.css$/ () "text/css"]
                           [#/\.html$/ () "text/html; charset=uft-8"])
               (%respond "200" ctype (file->string filename) oport))]
            [('plain obj) (%respond "200" "text/plain; charset=utf-8"
                                    (write-to-string obj display) oport)]
            [('json alist) (%respond "200" "application/json; charset=uft-8"
                                     (alist->json alist) oport)]
            [else (%respond "200" "text/html; charset=utf-8"
                            (tree->string body) oport)]))
        (request-status-set! req "200")
        req)
    (unless keepalive (socket-close (request-socket req)))))

(define (alist->json alist)
  (tree->string
   `("{",(intersperse
          ","
          (map (^p `(,(write-to-string (x->string (car p)))
                     ":",(write-to-string (cdr p))))
               alist))
     "}")))

;;;
;;; Handler mechanism
;;;

(define *handlers* (make-queue))

;; handler :: Request Path-RxMatch -> IO ()
(define-syntax define-http-handler
  (syntax-rules ()
    [(_ path-rx handler) (enqueue! *handlers* (cons path-rx handler))]))

(define (dispatch-handler req)
  (let1 path (request-path req)
    (or (any-in-queue (^p(cond [((car p) path) => (cut(cdr p)req <>)][else #f]))
                      *handlers*)
        (respond/ng req "404"))))

;;;
;;; Main loop
;;;

(define (start-http-server :key (host #f)
                                (port 8080)
                                (document-root ".")
                                (num-threads 5)
                                (max-backlog 10)
                                (log-to #f))
  (parameterize ([httpd-log-drain
                  (cond [(not log-to) #f]
                        [(is-a? log-to <log-drain>) log-to]
                        [else (make <log-drain> :path log-to :prefix "")])])
    (let* ([pool (tpool:make-thread-pool num-threads :max-backlog 10)]
           [tlog (kick-logger-thread pool)]
           [ssocks (make-server-sockets host port :reuse-addr? #t)])
      (unwind-protect
          (let1 sel (make <selector>)
            (dolist [s ssocks]
              (selector-add! sel (socket-fd s)
                             (lambda (fd condition)
                               (accept-client (socket-accept s) pool))
                             '(r)))
            (while #t (selector-select sel)))
        (log "terminating")
        (for-each socket-close ssocks)
        (tpool:terminate-all! pool 300)
        (thread-terminate! tlog)))))

(define (kick-logger-thread pool)
  (thread-start! (make-thread (cut logger pool))))

(define (accept-client csock pool)
  (unless (tpool:add-job! pool (cut handle-client csock) #t)
    (respond/ng (make-partial-request "too many request backlog" csock) "503")
    (socket-close csock)))

(define (handle-client csock)
  (guard (e [else (respond/ng (make-partial-request (~ e'message) csock) "500")])
    (let* ([iport (socket-input-port csock)]
           [line (read-line (socket-input-port csock))])
      (rxmatch-case line
        [test eof-object?
         (respond/ng (make-partial-request "client gone" csock) "400")]
        [#/^(GET|HEAD)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
         (receive (auth path q frag) (uri-decompose-hierarchical abs-path)
           (let ([params (cgi-parse-parameters :query-string (or q ""))]
                 [path (uri-decode-string path :cgi-decode #t)])
             (dispatch-handler (make-request line csock meth path params
                                             (rfc822-read-headers iport)))))]
        [#/^[A-Z]+.*/
         (respond/ng (make-request line csock "" "" '() '()) "501")]
        [else
         (respond/ng (make-request line csock "" "" '() '()) "400")]))))

;;;
;;; Logging
;;;

(define (logger pool)
  (guard (e [else (log "[internal] logger error: ~a" (~ e'message))])
    (let loop ()
      (let1 j (dequeue/wait! (~ pool'result-queue))
        (case (job-status j)
          [(done) (let1 r (job-result j)
                    (unless (request? r)
                      (error "some handler didn't return request:" r))
                    (log "~a: ~a \"~a\" ~a ~a ~a"
                         (logtime (job-acknowledge-time j))
                         (logip (request-remote-addr r))
                         (request-line r)
                         (request-status r)
                         (request-reply-size r)
                         (logdt (job-acknowledge-time j) (job-finish-time j))))]
          [(error) (log "[internal] job error: ~a" (~ (job-result j)'message))]
          [(killed) (log "[internal] job killed: ~a" (job-result j))]
          [else (log "[internal] unexpected job status: ~a" (job-status j))]))
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
  
;;;
;;; Built-in handlers
;;;

(define (file-handler :key (directory-index '("index.html" #t)))
  (lambda (req m) (%handle-file req m directory-index)))

(define (%handle-file req m dirindex)
  (let1 rpath (sys-normalize-pathname (request-path req) :canonicalize #t)
    (if (or (string-prefix? "/../" rpath)
            (string=? "/.." rpath))
      (respond/ng req "403")      ;do not allow path traversal
      (let1 fpath (sys-normalize-pathname #`",(docroot),rpath")
        (cond [(file-is-readable? fpath)
               (if (file-is-directory? fpath)
                 (%handle-directory req fpath rpath dirindex)
                 (respond/ok req `(file ,fpath)))]
              [(file-exists? fpath) (respond/ng req "403")]
              [else (respond/ng req "404")])))))

(define (%handle-directory req fpath rpath dirindex)
  (let loop ([ind dirindex])
    (match ind
      [() (respond/ng req "403")]
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
