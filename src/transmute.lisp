(in-package :cl-user)
(defpackage transmute
  (:use :cl :sxql :alexandria)
  (:import-from :cl-ppcre
                :scan
                :register-groups-bind)
  (:import-from :dbi
                :with-transaction
                :fetch)
  (:import-from :cl-fad
                :merge-pathnames-as-file
                :list-directory)
  (:import-from :datafly
                :*connection*
                :connect-cached
                :retrieve-one
                :execute)
  (:import-from :transmute.generator
                :gen-migration
                :*migration-dir*
                :*package-prefix*)
  (:export :*migration-dir*
           :*package-prefix*
           :gen-migration))

(in-package :transmute)

(annot:enable-annot-syntax)

(defun db (spec)
  (apply #'connect-cached spec))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defun ensure-schema-versions-table ()
  (execute
   (create-table (:schema_versions :if-not-exists t)
       ((version :type '(:varchar 255)
                 :primary-key t)))))

(defmacro with-schema-versions-table (&body body)
  `(progn
     (ensure-schema-versions-table)
     ,@body))

(defun get-current-version ()
  (with-schema-versions-table
      (let* ((query
              (select :version
                      (from :schema_versions)
                      (order-by (:desc :version))))
             (result (retrieve-one query)))
        (when result
          (parse-integer (getf result :version))))))

(defstruct migration-entry
  version
  filename
  package-name)

(defun pathname-filename (filepath)
  (format nil "~A.~A"
          (pathname-name filepath)
          (pathname-type filepath)))

(defun migration-filename-p (filename)
  (scan "^[0-9]+_.*\\.lisp$" filename))

(defun extract-version (migration-file-name)
  (register-groups-bind
   (version)
   ("^([0-9]+)_.*\\.lisp$" migration-file-name)
   (parse-integer version)))

(defun make-package-name (migration-file-name)
  (register-groups-bind
   (basename)
   ("^([0-9]+_.*)\\.lisp$" migration-file-name)
   (make-keyword
    (string-upcase
     (concatenate 'string (or *package-prefix* "") basename)))))

(defun gen-migration-entry-from-filename (filename)
  (let ((version (extract-version filename))
        (pname (make-package-name filename)))
    (make-migration-entry
     :version version
     :filename filename
     :package-name pname)))

(defun list-migration-entries ()
  (let ((dir-conts
         (list-directory *migration-dir*)))
    (loop
       for filepath in dir-conts
       for filename = (pathname-filename filepath)
       if (migration-filename-p filename)
       do
         (format t "~&found migration file ~A~%" filename)
       and
       collect (gen-migration-entry-from-filename filename) into entries
       end
       finally (progn
                 (sort entries #'> :key #'migration-entry-version)
                 (return entries)))))

(defun filter-migrations (pred)
  (let ((current-ver (get-current-version))
        (entries (list-migration-entries)))
    (format t "~&Current version: ~A~%" current-ver)
    (if current-ver
        (remove-if-not
         (lambda (e)
           (funcall pred (migration-entry-version e) current-ver))
         entries)
        entries)))

(defun pending-migrations ()
  (filter-migrations #'>))

(defmethod run-migration ((entry migration-entry))
  (let ((migration-file-path
         (merge-pathnames-as-file *migration-dir*
                                  (migration-entry-filename entry))))
    (format t "~&Load migration file: ~A~%" migration-file-path)
    (load migration-file-path))
  (with-transaction *connection*
    (funcall (ensure-symbol 'migrate
                            (migration-entry-package-name entry)))
    (execute
     (insert-into :schema_versions
                  (set= :version (migration-entry-version entry))))))

@export
(defun migrate (spec)
  (with-connection (db spec)
    (with-schema-versions-table
        (loop
           for entry in (pending-migrations)
           do
             (progn
               (format t "~&run migration file: ~A~%" (migration-entry-filename entry))
               (run-migration entry))))))
