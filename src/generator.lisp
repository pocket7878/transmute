(in-package :cl-user)

(defpackage :transmute.generator
  (:use :cl :alexandria)
  (:import-from :cl-fad
                :merge-pathnames-as-file)
  (:import-from :cl-emb
                :execute-emb)
  (:import-from :cl-ppcre
                :register-groups-bind)
  (:import-from :kebab
                :to-snake-case))

(in-package :transmute.generator)

(annot:enable-annot-syntax)

(defvar *migration-dir* #P"db/migrations/")

(defvar *package-prefix* nil)

(defvar *skeleton-directory*
  #.(asdf:system-relative-pathname
     :transmute
     #p"skeleton/"))

(defun template-pathname (filename)
  (merge-pathnames-as-file *skeleton-directory* filename))


#|
Automatically generate migration file under *migration-dir*

ex. 20150310172923_create-new-users.lisp

|#
;;Using timestamp as a migration version.
(defun gen-new-version ()
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format  nil
             "~4,'0',,D~2,'0',,D~2,'0',,D~2,'0',,D~2,'0',,D~2,'0',,D"
             year month date hour min sec)))

(defun gen-new-migration-file-path (version name)
  (format nil "~A_~A.lisp" version name))

(defun gen-new-package-name (version name)
  (make-keyword
   (string-upcase
    (format nil "~A~A_~A" (or *package-prefix* "") version name))))

(defun gen-new-migration-parameters (migration-name)
  (let* ((new-version (gen-new-version))
         (new-package-name (gen-new-package-name new-version migration-name))
         (new-filename
          (gen-new-migration-file-path new-version migration-name))
         (target-path
          (merge-pathnames-as-file *migration-dir* new-version new-filename)))
    (values new-version new-package-name target-path)))

(defun write-emb (emb-path target-path &key (env nil))
  (with-open-file (stream target-path :direction :output :if-exists :supersede)
    (write-sequence
     (cl-emb:execute-emb
      emb-path
      :env env)
     stream)))

(defmacro define-migration-generator (name template-name regex binds)
  (let* ((fname-gen (gensym))
         (ver-gen (gensym))
         (pname-gen (gensym))
         (tpath-gen (gensym))
         (parameter-list
          `(:package-name
            ,pname-gen
            :version ,ver-gen
            ,@(loop for b in binds
                   append (list (make-keyword b) b)))))
    `(defun ,name (,fname-gen)
       (register-groups-bind ,binds
           (,regex ,fname-gen)
         (multiple-value-bind (,ver-gen ,pname-gen ,tpath-gen) (gen-new-migration-parameters ,fname-gen)
           (format t "~&writing ~A~%" ,tpath-gen)
           (write-emb
            (template-pathname ,template-name)
            ,tpath-gen
            :env (list ,@parameter-list)))))))

(define-migration-generator
    create-table-generator
    "create-table-migration.lisp"
  "^create_([^_]*)_table$" (table-name))

(define-migration-generator
    add-column-generator
    "add-column-migration.lisp"
  "^add_([^_]*)_to_([^_]*)$" (column-name table-name))

(define-migration-generator
    remove-column-generator
    "remove-column-migration.lisp"
  "^remove_([^_]*)_from_([^_]*)$" (column-name table-name))

(define-migration-generator
    empty-migration-generator
    "migration.lisp"
  "^.*$" ())

@export
(defun gen-migration (name)
  (let ((snake-name (to-snake-case name)))
    (or
     (create-table-generator snake-name)
     (add-column-generator snake-name)
     (remove-column-generator snake-name)
     (empty-migration-generator snake-name))))

