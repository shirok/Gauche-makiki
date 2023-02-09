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

;; EXPERIMENTAL

;; Load and run makiki service in subthread for development

;; We assume the server script has 'main' procedure that
;; calls start-http-server to enter server loop.  START-SERVER loads
;; that script in an isolated module and calls 'main' in a thread.
;; This has a few drawbacks.
;; - There's no consistent way to specify various options of
;;   the server, e.g. ports.  It all depends on how the script
;;   interpret its arguments, which depends on the script.
;; - There's no way to stop the server that's guaranteed to be safe.
;; - Reloading doesn't stop the server thread.  Change of handlers
;;   would be reflected, but changes in the main procedure, or the
;;   app structure passed to the start-http-server, won't take effect.
;;
;; The Right thing would be to define a proper protocol in start-http-server
;; so that it can be run in 'dev mode'.

(define-module makiki.dev
  (use gauche.threads)
  (use makiki)
  (export start-server! stop-server!))
(select-module makiki.dev)

;; Those APIs are supposed to be called interactively in REPL, so
;; we don't care about thread safety for now.

(define *server-file* #f)
(define *server-module* #f)
(define *server-thread* #f)

;; a hidden parameter to stop the server loop
(define dev-cch (with-module makiki dev-control-channel))

;; API
(define (start-server! :optional (path *server-file*))
  (unless path
    (error "Server file path rquired for the first call."))
  (set! *server-file* path)
  (unless *server-module*
    (set! *server-module* (make-module '#:makiki-dev)))
  ;; Kludge - clear the existing handlers.  This isn't a public API.
  ((with-module makiki clear-handlers!))
  (load (sys-normalize-pathname path :absolute #t :expand #t)
        :environment *server-module*)
  (unless *server-thread*
    (dev-cch (make-server-control-channel))
    (set! *server-thread*
          (thread-start!
           (make-thread (cut %run-server! *server-module* path)))))
  *server-thread*)

(define (%run-server! mod server-file)
  (cond [(global-variable-ref mod 'dev-main #f)
         => (^[dev-main] (dev-main `(,server-file)))]
        [(global-variable-ref mod 'main #f)
         => (^[main] (main `(,server-file)))]
        [else (error "Can't find 'dev-main' nor 'main' in the server script:"
                     server-file)]))

;; API
(define (stop-server!)
  (when *server-thread*
    (terminate-server-loop (dev-cch) 0)
    (sys-nanosleep #e1e8)               ;for now; better to handshake
    (%shutdown-server! *server-module*)
    (dev-cch #f)
    (thread-terminate! *server-thread*)
    (set! *server-thread* #f))
  #t)

(define (%shutdown-server! mod)
  (cond [(global-variable-ref mod 'dev-shutdown #f)
         => (^[dev-shutdown] (dev-shutdown))]
        [else #f]))
