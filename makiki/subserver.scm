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

;; Run Makiki server as a subprocess.  Useful to run tests involving
;; an http server.

(define-module makiki.subserver
  (use gauche.process)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.vport)
  (use data.queue)
  (export call-with-httpd call-with-httpd/wait))
(select-module makiki.subserver)

;; TRANSOENT:
;; This will eventually be in Gauche.  To make this module work with
;; the current release of Gauche, we duplicate the code here.
;; Remove this later.
(define (open-tapping-port iport oport :key (close-output #f))
  (define mtq (make-mtqueue))
  (define tee-closed #f)
  (define straight-closed #f)
  (define (filler buf)
    (let ([len (u8vector-length buf)]
          [data (dequeue/wait! mtq)])     ;this may block
      (cond [(eof-object? data) data]
            [(<= (u8vector-length data) len)
             (u8vector-copy! buf 0 data 0)
             (u8vector-length data)]
            [else
             (u8vector-copy! buf 0 data 0 len)
             (queue-push/wait! mtq (uvector-alias <u8vector> data len))
             len])))
  (define (closer)
    (set! tee-closed #t))
  (define (handler)
    (let loop ()
      ;; We ignore error; it prevents the thread to exit inadvertently.
      ;; However, it has a risk to go into busy loop if error condition
      ;; persists.  We'll revisit this later.
      (unless (guard [e (else #f)]
                (let1 data (read-uvector <u8vector> 4096 iport)
                  (unless tee-closed
                    (enqueue/wait! mtq data))
                  (unless straight-closed
                    (if (eof-object? data)
                      (begin
                        (set! straight-closed #t)
                        (when close-output
                          (close-output-port oport)))
                      (begin
                        (write-uvector data oport)
                        (flush oport))))
                  (when (eof-object? data)
                    (close-input-port iport))
                  (and tee-closed straight-closed)))
        (loop))))
  (thread-start! (make-thread handler))
  (make <buffered-input-port> :fill filler :close closer))

;; SERVER-SCRIPT must call start-http-server with access-log to stdout.
(define (call-with-httpd server-script proc)
  (let* ([p (run-process `(gosh "-I." ,server-script "--port" 0)
                            :output :pipe :error :pipe :wait #f)]
         [port (or (get-port p)
                   (error "Failed to start server script: " server-script))]
         [host #"localhost:~port"])
    (unwind-protect (proc p host port)
      (process-kill p))))

(define (call-with-httpd/wait server-script proc)
  (let* ([p (run-process `(gosh "-I." ,server-script "--port" 0)
                         :output :pipe :error :pipe :wait #f)]
         [port (or (get-port p)
                   (error "Failed to start server script: " server-script))]
         [host #"localhost:~port"])
    (proc p host port)
    (process-wait p)
    (sys-wait-exit-status (process-exit-status p))))

(define (get-port process)
  (let1 port (open-tapping-port (process-error process)
                                (current-output-port))
    (rxmatch-case (read-line port)
      [#/started on \(.*:(\d+)\)/i (_ pp)
                 (close-input-port port)
                 (x->integer pp)]
      [else (get-port process)])))
