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
   (alter-table :<% @var table-name %>
     (add-column :<% @var column-name %>))))
