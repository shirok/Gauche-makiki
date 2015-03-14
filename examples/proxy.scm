#!/usr/bin/env gosh

;;
;; Rudimental proxy.  Handles CONNECT request as well.
;;
;; Note: These days, one webpage pulls tons of network resources
;; and browers rely on concurrent, streamed loading (they start
;; rendering as soon as partial data is arrived.)  This program
;; fetches entire content from the server first, then send it back
;; to the requester.  So you'll experience a lot more delay in browsing.
;; For the practical use, you want piping---pass the data from ther
;; server to the requester in small chunks.

(use gauche.net)
(use gauche.parseopt)
(use rfc.http)
(use rfc.uri)
(use rfc.822)
(use util.match)

(add-load-path ".." :relative)
(use makiki)
(use makiki.connect) ; for connect-dispatcher

(define (main args)
  (let-args (cdr args) ([port "p|port=i" 8016])
    (add-method-dispatcher! 'CONNECT connect-handler/local-only)
    ;; NB: Modern webpages contain tons of small https references
    ;; (ads, trackers etc.) and tends to require lots of CONNECT
    ;; in parallel; so we increase num-threads.
    (start-http-server :access-log #t :error-log #t :port port
                       :num-threads 50 :app-data #f))
  0)

;; If you need just CONNECT feature, the following line is enough.
;;  (add-method-dispatcher! 'CONNECT connect-dispatcher)
;; But we don't want an open proxy, so the following code
;; check if the client is on the same machine.

(define (local-connection? csock)
  (let1 client-addr (socket-getpeername csock)
    (or (and (is-a? client-addr <sockaddr-in>)
             (eqv? #x7f (ash (sockaddr-addr client-addr) -24)))
        (cond-expand
         [gauche.net.ipv6
          (and (is-a? client-addr <sockaddr-in6>)
               (or (eqv? 1 (sockaddr-addr client-addr))
                   (eqv? #xffff7f   ; ipv4-mapped
                         (logand (ash (sockaddr-addr client-addr) -24)
                                 #xffffff))))]
         [else #f]))))

(define (connect-handler/local-only req app)
  (if (local-connection? (request-socket req))
    (connect-dispatcher req app)
    (respond/ng req 403)))

;;
;; Request forwarding
;;  We buffer remote reply entirely for simplicity; for practical use,
;;  you may not want to do this.

(define-http-handler #/^.*/
  (^[req app]
    (match-let1 (server path) (uri-ref (request-uri req) '(host+port path+query))
      (receive (status headers body)
          (guard (e [else
                     (error-log "Error while fetching remote data: ~a"
                                (~ e'message))
                     (values "500" '() "")])
            (http-get server path))
        (dolist [h headers]
          (unless (member (car h) '("content-type" "transfer-encoding"))
            (response-header-push! req (car h) (cadr h))))
        (if (equal? status "200")
          (respond/ok req body
                      :content-type (rfc822-header-ref headers "content-type"
                                                       "binary/octet-stream"))
          (respond/ng req (x->integer status) :body body))))))
