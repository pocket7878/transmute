(in-package :cl-user)
(defpackage :<% @var package-name %>
   (:use :cl :sxql))

(in-package :<% @var package-name %>)

;;Migraton function.
(defun migrate ()
  "Migrates the database up to version <% @var version %>."
  ;;Bla bla bla
  )
