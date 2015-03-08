#|
  This file is a part of transmute project.
  Copyright (c) 2015 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage transmute-test-asd
  (:use :cl :asdf))
(in-package :transmute-test-asd)

(defsystem transmute-test
  :author "Masato Sogame"
  :license "LLGPL"
  :depends-on (:transmute
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "transmute"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
