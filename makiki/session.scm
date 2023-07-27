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

;; Usage:
;;   1. Inherit <session-mixin> class by your app class.  Then a session-bin
;;      is automatically available.
;;   2. Create a session bin and keep it somewhere.

(define-module makiki.session
  (use gauche.threads)
  (use data.cache)
  (use data.random)
  (export make-session-bin <session-mixin>
          session-ref session-set! session-delete!))
(select-module makiki.session)

(define-class <session-bin> ()
  (;; all slots are private
   (%cache :init-keyword :cache)))

(define-constant *default-timeout* (* 60 60 2))

(define %key-gen (strings-of 64 #[\w]))

;; API
(define (make-session-bin :optional (timeout *default-timeout*))
  (make <session-bin>
    :cache (make-ttlr-cache timeout :comparator string-comparator)))

;; API
(define-class <session-mixin> ()
  (;; This mixin class recognizes :session-timeout init option, which is
   ;; used to initialize session-bin.
   (session-bin)))

(define-method initialize ((obj <session-bin>) initargs)
  (next-method)
  ;; We wrap the session-bin with atom, for we don't know how application
  ;; handles the locking.  It is redundant if the application locks entire
  ;; app object, but that should be minor.
  (set! (~ obj'session-bin)
        (atom
         (make-ttlr-cache (get-keyword :session-timeout initargs
                                       *default-timeout*)))))

;; API
(define-method session-ref ((app <session-mixin>) key :optional default)
  (atomic (~ app'session-bin)
          (cut session-ref <> key default)))
(define-method session-ref ((bin <session-bin>) key :optional (default #f))
  (if key
    (cache-lookup! (~ bin'%cache) key default)
    default))

;; API
;;   KEY can be #f, in which case a new session key is generated.
;;   Returns the session key.
(define-method session-set! ((app <session-mixin>) key data)
  (atomic (~ app'session-bin)
          (cut session-set! <> key data)))
(define-method session-set! ((bin <session-bin>) key data)
  (rlet1 key (or key (%key-gen))
    (cache-write! (~ bin'%cache) key data)))

;; API
(define-method session-delete! ((app <session-mixin>) key)
  (atomic (~ app'session-bin)
          (cut session-delete! <> key)))
(define-method session-delete! ((bin <session-mixin>) key)
  (cache-evict! (~ bin'%cache) key)
  (undefined))
