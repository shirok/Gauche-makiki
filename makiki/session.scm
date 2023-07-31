;;;
;;;   Copyright (c) 2023 Shiro Kawai  <shiro@acm.org>
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

;; Support common session handling

(define-module makiki.session
  (use gauche.threads)
  (use data.cache)
  (use data.random)
  (use makiki)
  (export make-session-bin session-key
          session-ref session-set! session-delete!
          with-session))
(select-module makiki.session)

(define-class <session-bin> ()
  (;; all slots are private
   (%timeout :init-keyword :timeout :immutable #t)
   (%bin :init-keyword :bin)))

(define-constant *default-timeout* (* 60 60 2))

(define %key-gen (strings-of 64 (chars$ #[\w])))

;; API
(define (make-session-bin :optional (timeout *default-timeout*))
  (make <session-bin>
    :timeout timeout
    :bin (atom (make-ttlr-cache timeout :comparator string-comparator))))

;; Global session bin for the default.  An application can override
;; it with `with-session`.
(define %the-session-bin
  (make-parameter (make-session-bin)))

;; `with-session` binds this parameter to the session key.  #f means
;; no session.
(define session-key (make-parameter #f))

;; API
(define (session-ref :optional (default #f))
  (if-let1 key (session-key)
    (atomic (~ (%the-session-bin)'%bin) (cut cache-lookup! <> key default))
    default))

;; API
;;   If there's no active key, a new key is created.
(define (session-set! data)
  (rlet1 key (or (session-key)
                 (rlet1 k (%key-gen)
                   (session-key k)))
    (atomic (~ (%the-session-bin)'%bin) (cut cache-write! <> key data))))

;; API
(define (session-delete!)
  (and-let1 key (session-key)
    (atomic (~ (%the-session-bin)'%bin) (cut cache-evict! <> key)))
  (session-key #f)
  (undefined))

;; API
(define (with-session handler
                      :optional (bin-getter %the-session-bin)
                                (cookie-name "makiki-session"))
  (^[req app]
    (let ([bin (bin-getter)]
          ;; NB: cookie := (name value opts ...)
          [cookie (request-cookie-ref req cookie-name)])
      ;; We need to determine response cookie after the body of the
      ;; handler is executed, so we use respond-callback.
      (respond-callback-add!
       (^[req code content-type]
         (cond
          [(session-key)
           ;; If session-key is set, we set it to the cookie.  This covers
           ;; both existing sessions and new sessions.
           => (^[key]
                ($ response-cookie-add! req cookie-name key
                   :max-age (~ bin'%timeout)))]
          [cookie
           ;; If request had the key but not in session-key, the handler
           ;; deleted the session.
           (response-cookie-delete! req cookie-name)])))
      ;; Run handler with session-key bound to the request key
      (parameterize ((session-key (and cookie (cadr cookie))))
        (handler req app)))))
