(define-gauche-package "Gauche-makiki"
  :version "0.4"
  :description "Simple multithreaded http server"
  :require (("Gauche" (>= "0.9.7")))
  :providing-modules (makiki makiki.cgi makiki.connect makiki.dev)
  :maintainers ("shiro@acm.org")
  :authors ("Shiro Kawai <shiro@acm.org>")
  :homepage "https://github.com/shirok/Gauche-makiki"
  :repository "https://github.com/shirok/Gauche-makiki"
  :licenses ("BSD")
  )
