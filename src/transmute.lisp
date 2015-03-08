(in-package :cl-user)
(defpackage transmute
  (:use :cl :sxql :alexandria)
  (:import-from :cl-ppcre
                :register-groups-bind)
  (:import-from :dbi
                :fetch)
  (:import-from :cl-fad
                :merge-pathnames-as-file
                :list-directory)
  (:import-from :datafly
                :*connection*
                :connect-cached
                :execute))
(in-package :transmute)

(defun db (spec)
  (apply #'connect-cached spec))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defun ensure-schema-migrations-table ()
  (execute
   (create-table (:schema-migrations :if-not-exists t)
       ((version :type 'string
                 :primary-key t)))))

(defmacro with-schema-migrations-table (&body body)
  `(progn
     (ensure-schema-migrations-tabl)
     ,@body))

(defun get-current-version ()
  (with-schema-migrations-table
      (let* ((result
              (execute
               (yield
                (select :version
                  (from :schema-migrations)
                  (order-by (:desc version))
                  (limit 1)))))
             (row (fetch result)))
        (when row
          (getf (car row) :version)))))

(defvar *migration-dir* #P"migrations/")

(defvar *package-prefix* nil)

(defstruct migration-entry
  version
  filename
  package-name)

(defun extract-version (migration-file-name)
  (register-groups-bind
   (version)
   ("([0-9]+)_.*\\.lisp" migration-file-name)
   (parse-integer version)))

(defun make-package-name (migration-file-name)
  (register-groups-bind
   (basename)
   ("([0-9]+_.*)\\.lisp" migration-file-name)
   (symbolicate basename)))

(defun gen-migration-entry-from-filename (filename)
  (let ((version (extract-version filename))
        (pname (make-package-name filename)))
    (make-migration-entry
     version
     filename
     pname)))

(defun list-migration-entries ()
  (let ((dir-conts
         (list-directory *migration-dir*)))
    (loop
       for filename in dir-conts
       collect (gen-migration-entry-from-filename filename) into entries
       finally (sort entries #'> :key #'migration-entry-version))))

(defun filter-migrations (pred)
  (let ((current-ver (get-current-version))
        (entries (list-migration-entries)))
    (remove-if
     (lambda (e)
       (funcall pred (migration-entry-version e) current-ver))
     entries)))

(defmacro defmigrate (&optional doc &body body)
  `(defvar
       *migration*
     (lambda ()
       ,@body)))

(defmethod run-migration (migration-entry entry)
  (load (merge-pathnames-as-file
         *migration-dir*
         (migration-entry-filename entry)))
  (funcall (ensure-symbol *migration*
                          (migration-entry-package-name entry))))

(defun migrate (spec)
  (with-connection (db spec)
    (loop
       for entry in (filter-migrations #'>)
       do (run-migration entry))))
