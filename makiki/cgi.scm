;;;
;;; makiki.cgi - cgi adaptor
;;;
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

;; This module is to use cgi scripts written in Gauche inside
;; makiki server.  It is mainly for easier transition from cgi
;; to application server with least effort.
;; If you use Gauche for both, you can directly write handlers in
;; makiki, so there's little use of this module for new projects.

(define-module makiki.cgi
  (use gauche.vport)
  (use gauche.parameter)
  (use gauche.uvector)
  (use gauche.net)
  (use srfi-13)
  (use rfc.822)
  (use www.cgi)
  (use makiki)
  (export cgi-handler cgi-script))
(select-module makiki.cgi)

;; API
;;
;; This can be used to call a cgi program's main procedure PROC,
;; with setting cgi metavariables and current i/o's.
;; We don't support http authentications yet.
;;
;; SCRIPT-NAME should be an absolute path of the script that appear in
;; URL.  That is, if you want to make the script invoked as
;; http://example.com/foo/bar/baz.cgi, give "/foo/bar/baz.cgi".
;; It is necessary to calculate PATH_INFO properly.
(define (cgi-handler proc :key (script-name "") (forwarded #f))
  (^[req app]
    (let1 out (open-output-string)
      (with-input-from-port (request-iport req)
        (^[]
          (let1 r (parameterize
                      ([cgi-metavariables
                        (get-cgi-metavariables req script-name forwarded)]
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
;;
;; If FORWARDED is true, some CGI metavariables are overridden with
;; X-Forwarded-* header values.  They are useful if Makiki is running
;; behind a reverse proxy.
;;   X-Forwarded-Host   SERVER_NAME
;;   X-Forwarded-Port   SERVER_PORT
;;   X-Forwarded-For    REMOTE_ADDR
(define (cgi-script file :key (entry-point 'main)
                              (script-name "")
                              (load-every-time #f)
                              (forwarded #f))
  (define (load-script) ;returns entry point procedure
    (let1 mod (make-module #f)
      (load file :environment mod)
      (global-variable-ref mod entry-point)))

  (if load-every-time
    (^[req app]
      ((cgi-handler (load-script) :script-name script-name :forwarded forwarded)
       req app))
    (cgi-handler (load-script) :script-name script-name :forwarded forwarded)))

;; Sets up cgi metavariables.
(define (get-cgi-metavariables req script-name forwarded)
  (cond-list
   ;; AUTH_TYPE - not supported yet
   [(request-header-ref req "content-length")
    => (^v (list "CONTENT_LENGTH" (x->string v)))]
   [(request-header-ref req "content-type")
    => (^v (list "CONTENT_LENGTH" (x->string v)))]
   [#t `("GATEWAY_INTERFACE" "CGI/1.1")]
   [#t `("PATH_INFO"
         ,(and (string-prefix? script-name (request-path req))
               (< (string-length script-name)
                  (string-length (request-path req)))
               (string-drop (request-path req)
                            (string-length script-name))))]
   [#t `("PATH_TRANSLATED" ;todo - flexible path trans.
         ,(string-append (document-root) (request-path req)))]
   [#t `("QUERY_STRING" ,(or (request-query req) ""))]
   [#t `("REMOTE_ADDR" ,(or (and forwarded
                                 (request-header-ref req "x-forwarded-for"))
                            (let1 addr (request-remote-addr req)
                              (inet-address->string (sockaddr-addr addr)
                                                    (case (sockaddr-family addr)
                                                      [(inet)  AF_INET]
                                                      [(inet6) AF_INET6]
                                                      [else AF_INET])))))]
   ;; REMOTE_HOST - not supported
   ;; REMOTE_IDENT - not supported
   ;; REMOTE_USER - not supported
   [#t `("REQUEST_METHOD" ,(x->string (request-method req)))]
   [#t `("SCRIPT_NAME" ,script-name)]
   [#t `("SERVER_NAME" ,(or (and forwarded
                                 (request-header-ref req "x-forwarded-host"))
                            (request-server-host req)))]
   [#t `("SERVER_PORT" ,(or (and forwarded
                                 (or
                                  (request-header-ref req "x-forwarded-port")
                                  (if (equal? (request-header-ref req "x-forwarded-proto") "https")
                                    443
                                    80)))
                            (request-server-port req)))]
   [#t `("SERVER_PROTOCOL" "HTTP/1.1")]
   [#t `("SERVER_SOFTWARE" ,(http-server-software))]
   [(request-header-ref req "referer") => (^r `("HTTP_REFERER" ,r))]
   [(request-header-ref req "cookie") => (^c `("HTTP_COOKIE" ,c))]
   ;; NB: For now, Makiki itself isn't capable of serving https, so
   ;; this only happens when it is behind a reverse proxy.
   [(and forwarded
         (equal? (request-header-ref req "x-forwarded-proto") "https"))
    '("HTTPS" "on")]
   ))
