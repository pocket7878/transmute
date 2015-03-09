(in-package :cl-user)
(defpackage :transmute.clack-pending-migration-error
  (:use :cl
        :clack
        :clack.response)
  (:import-from :transmute
                :with-connection
                :with-schema-versions-table)
  (:import-from :clack.middleware.dbi
                :*db*)
  (:import-from :caveman.middleware.dbimanager
                :connect-db)
  (:import-from :clack.component
                :component-designator))

(in-package :transmute.clack-pending-migration-error)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-migration-check-middleware> (<middleware>)
  ((block-app :initarg :block-app
              :type component-designator
              :initform #'return-500
              :accessor block-app))
  (:documentation "Clack Middleware for check pending migration."))

(defmethod call ((this <clack-migration-check-middleware>) env)
  (with-connection *db*
    (with-schema-versions-table
        (if (null (pending-migrations))
            (call-next this env)
            (call (block-app this) env)))))

@export 
(defclass <caveman-migration-check-middleware> (<middleware>)
  ((database-name :initarg :database-name
                  :initform nil
                  :accessor database-name)
   (block-app :initarg :block-app
              :type component-designator
              :initform #'return-500
              :accessor block-app))
  (:documentation "Clack Middleware for check pending migration.
This is similar to `<clack-pending-migration-check-middleware>, except this depends on `<caveman-middleware-dbimanager>`."))

(defmethod call ((this <caveman-migration-check-middleware>) env)
  (with-connection (let ((dname (database-name this)))
                     (if dname
                         (connect-db dname)
                         (connect-db)))
    (with-schema-versions-table
        (if (null (pending-migrations))
            (call-next this env)
            (call (block-app this) env)))))

(defun return-500 (env)
  @ignore env
  '(500
    (:content-type "text/plain"
     :conent-length 48)
    ("Internal Server error: pending migration exists.")))
