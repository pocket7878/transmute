(in-package :cl-user)
(defpackage :<% @var package-name %>
  (:use :cl :sxql)
  (:import-from :datafly
                :execute))

(in-package :<% @var package-name %>)

;;Migraton function.
(defun migrate ()
  "Migrates the database up to version <% @var version %>."
  (execute
   (create-table :<% @var table-name %>
                 (
                  ;;Listing slots here..
                  ))))
