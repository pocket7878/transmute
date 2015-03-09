#|
  This file is a part of clack-migration-checker project.
  Copyright (c) 2015 Masato Sogame (poketo7878@gmail.com)
|#

#|
  Author: Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-migration-checker-asd
  (:use :cl :asdf))
(in-package :clack-migration-checker-asd)

(defsystem clack-migration-checker
  :version "0.1"
  :author "Masato Sogame"
  :license "LLGPL"
  :depends-on (:clack
               :clack-middleware-dbi
               :caveman-middleware-dbimanager
               :transmute)
  :components ((:module "src"
                :components
                ((:file "clack-migration-check"))))
  :description "")
