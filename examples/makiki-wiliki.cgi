#!/usr/bin/env gosh

;; This is a cgi script to run wiliki.
;; This can also be included from maiki-wiliki.scm.

(use wiliki)

(define (main args)
  (wiliki-main
   (make <wiliki>
     :db-path "/tmp/wikidata.dbm" ;change this to your suitable path
     :top-page "WiLiKi"
     :title "Wiliki"
     :description "WiLiKi - Wiki in Scheme"
     :style-sheet "wiliki.css"
     :log-file "wikidata.log"
     :event-log-file "wiliki.events.log"
     :language 'en
     :charsets '((jp . utf-8) (en . utf-8))
     :image-urls '()
     :debug-level 0
     )))

;; Local variables:
;; mode: scheme
;; end:
