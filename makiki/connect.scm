;;;
;;;   Copyright (c) 2015 Shiro Kawai  <shiro@acm.org>
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

;; An add-on to support CONNECT method

;; To enable CONNECT method, you can just say
;;
;;   (add-method-dispatcher! 'CONNECT connect-dispatcher)
;;
;; But this will be open proxy. To restrict connection, see
;; examples/proxy.scm

(define-module makiki.connect
  (use gauche.selector)
  (use gauche.uvector)
  (use gauche.net)
  (use makiki)
  (export connect-dispatcher))
(select-module makiki.connect)

(define (connect-dispatcher req app)
  (define-values (host port) ; CONNECT has destination in request-uri
    (if-let1 m (#/:(\d+)$/ (request-uri req))
      (values (m'before) (x->integer (m 1)))
      (values (request-uri req) 80)))
  (define csock (request-socket req))
  (define selector (make <selector>))
  (define count 2)
  (define (pipe buf ip op dest-sock)
    (let1 r (read-uvector! buf ip)
      (if (eof-object? r)
        (begin
          (selector-delete! selector ip #f #f)
          (dec! count)
          (close-input-port ip)
          (close-output-port op)
          (guard (e [else #f])
            (socket-shutdown dest-sock SHUT_WR)))
        (write-uvector buf op 0 r))))
  (define (tunnel-loop csock rsock)
    (define buf0 (make-u8vector 4096))
    (define buf1 (make-u8vector 4096))
    ($ selector-add! selector (socket-input-port csock)
       (^[input flag]
         (pipe buf0 (socket-input-port csock) (socket-output-port rsock) rsock))
       '(r))
    ($ selector-add! selector (socket-input-port rsock)
       (^[input flag]
         (pipe buf1 (socket-input-port rsock) (socket-output-port csock) csock))
       '(r))
    (do () [(= count 0)]
      (selector-select selector 1)))

  (let1 rsock (make-client-socket host port)
    (unwind-protect
        (begin
          (access-log "CONNECT (~a) starting" (request-uri req))
          (display "HTTP/1.1 200 OK\r\n\r\n" (socket-output-port csock))
          (flush (socket-output-port csock))
          (tunnel-loop csock rsock))
      (access-log "CONNECT (~a) shutting down" (request-uri req))
      (socket-close rsock))
    req))
