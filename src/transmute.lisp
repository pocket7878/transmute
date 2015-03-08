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
                :execute))
(in-package :transmute)

(annot:enable-annot-syntax)

(defun db (spec)
  (apply #'connect-cached spec))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defun ensure-schema-migrations-table ()
  (execute
   (create-table (:schema_migrations :if-not-exists t)
       ((version :type 'string
                 :primary-key t)))))

(defmacro with-schema-migrations-table (&body body)
  `(progn
     (ensure-schema-migrations-table)
     ,@body))

(defun get-current-version ()
  (with-schema-migrations-table
      (let* ((result
              (execute
               (yield
                (select :version
                  (from :schema_migrations)
                  (order-by (:desc :version))
                  (limit 1))))))
        (when result
          (getf (car result) :version)))))

(defvar *migration-dir* #P"db/migrations/")

(defvar *package-prefix* nil)

(defstruct migration-entry
  version
  filename
  package-name)

(defun pathname-filename (filepath)
  (format nil "~A.~A"
          (pathname-name filepath)
          (pathname-type filepath)))

(defun migration-filename-p (filename)
  (scan "[0-9]+_.*\\.lisp" filename))

(defun extract-version (migration-file-name)
  (register-groups-bind
   (version)
   ("([0-9]+)_.*\\.lisp" migration-file-name)
   (parse-integer version)))

(defun make-package-name (migration-file-name)
  (register-groups-bind
   (basename)
   ("([0-9]+_.*)\\.lisp" migration-file-name)
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
        (remove-if
         (lambda (e)
           (funcall pred (migration-entry-version e) current-ver))
         entries)
        entries)))

@export
(defmacro defmigrate (&optional doc &body body)
  `(defun migrate ()
     ,@body))

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
     (insert-into :schema_migrations
                  (set= :version (migration-entry-version entry))))))

(defun gen-new-version ()
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format  nil
             "~4,'0',,D~2,'0',,D~2,'0',,D~2,'0',,D~2,'0',,D~2,'0',,D"
             year month date hour min sec)))

@export
(defvar *skeleton-directory*
  #.(asdf:system-relative-pathname
     :transmute
     #p"skeleton/"))

(defun template-pathname (filename)
  (merge-pathnames-as-file *skeleton-directory* filename))

(defun gen-new-migration-file-path (version name)
  (format nil "~A_~A.lisp" version name))

(defun gen-new-package-name (version name)
  (make-keyword
   (string-upcase
    (format nil "~A~A_~A" (or *package-prefix* "") version name))))


@export
(defun gen-new-migration (name)
  (let* ((new-version (gen-new-version))
         (new-package-name (gen-new-package-name new-version name))
         (new-filename (gen-new-migration-file-path new-version name))
         (target-path
          (merge-pathnames-as-file *migration-dir* new-filename)))
    (format t "~&writing ~A~%" target-path)
    (with-open-file (stream target-path :direction :output :if-exists :supersede)
      (write-sequence
       (cl-emb:execute-emb
        (template-pathname "migration.lisp")
        :env `(:package-name
               ,new-package-name
               :version
               ,new-version))
       stream))))

@export
(defun migrate (spec)
  (with-connection (db spec)
    (with-schema-migrations-table
        (loop
           for entry in (filter-migrations #'>)
           do
             (progn
               (format t "~&run migration file: ~A~%" (migration-entry-filename entry))
               (run-migration entry))))))
