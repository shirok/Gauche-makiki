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
  (use text.tree)
  (use file.util)
  (use rfc.822)
  (use rfc.uri)
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
  `(log-format (httpd-log-drain) ,fmt ,@args))

;;;
;;; Request packet
;;;

(define-record-type request #t #t
  socket                                ; client socket
  method                                ; request method
  path                                  ; request path
  params                                ; query parameters
  headers)                              ; request headers

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

(define (respond/ng req/oport code :optional (keepalive #f))
  (let ([port (if (request? req/oport)
                (request-oport req/oport)
                req/oport)]
        [desc (hash-table-get *status-code-map* code "")])
    (begin0 (%respond code "text/plain; charset=utf-8" desc port)
      (when (and (request? req/oport) (not keepalive))
        (socket-close (request-socket req/oport))))))

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
                            (tree->string body) oport)])))
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
                                (log-drain #f))
  (parameterize ([httpd-log-drain (if (is-a? log-drain <log-drain>)
                                    log-drain
                                    (make <log-drain>
                                      :path log-drain :program-name "makiki"))])
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
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (let1 r (dequeue/wait! (~ pool'result-queue))
          (log-format "~a ~s" (job-status r) (job-result r))
          (loop)))))))

(define (accept-client csock pool)
  (unless (tpool:add-job! pool (cut handle-client csock))
    (respond/ng (socket-output-port csock) "503")
    (socket-close csock)))

(define (handle-client csock)
  (let1 oport (socket-output-port csock)
    (guard (e [else (respond/ng oport "500")])
      (match (get-request (socket-input-port csock))
        ['bad-request (respond/ng oport "400")]
        ['not-implemented (respond/ng oport "501")]
        [(meth path params . headers)
         (dispatch-handler (make-request csock meth path params headers))]
        [other (respond/ng oport "500")]))))

(define (get-request iport)
  (rxmatch-case (read-line iport)
    [test eof-object? 'bad-request]
    [#/^(GET|HEAD)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
     (receive (auth path q frag) (uri-decompose-hierarchical abs-path)
       (let1 params (cgi-parse-parameters :query-string (or q ""))
         (list* meth path params (rfc822-read-headers iport))))]
    [#/^[A-Z]+/ () 'not-implemented]
    [else 'bad-request]))

;;;
;;; Built-in handlers
;;;

(define (file-handler :key (directory-index '("index.html" #t)))
  (lambda (req m) (%handle-file req m directory-index)))

(define (%handle-file req m dirindex)
  (let1 cpath (sys-normalize-pathname (request-path req) :canonicalize #t)
    (if (or (string-prefix? "/../" cpath)
            (string=? "/.." cpath))
      (respond/ng req "403")      ;do not allow path traversal
      (let1 fpath (sys-normalize-pathname #`",(docroot),cpath")
        (cond [(file-is-readable? fpath)
               (if (file-is-directory? fpath)
                 (%handle-directory path dirindex)
                 (respond/ok req `(file ,fpath)))]
              [(file-exists? fpath) (respond/ng req "403")]
              [else (respond/ng req "404")])))))

(define (%handle-directory path dirindex)
  (let loop ([ind dirindex])
    (match ind
      [() (respond/ng req "403")]
      [(#t . _) (%index-directory path)]
      [(name . rest) (let1 f (build-path fpath name)
                       (if (file-is-readable? f)
                         (respond/ok req `(file ,f))
                         (loop rest)))])))
