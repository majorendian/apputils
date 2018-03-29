(defpackage :apputils
  (:use :cl)
  (:export
    define-config-dir
    define-config-file
    mkdir-if-not-exists
    with-write-config
    ))

(in-package :apputils)

(defun dir-ending ()
  #+:windows "\\"
  #-:windows "/")

(defun home-directory ()
  #-:windows (concatenate 'string (sb-posix:getenv "HOME") (dir-ending))
  #+:windows (concatenate 'string (sb-posix:getenv "USER") (dir-ending)))

;#+:windows
;windows specific
(defun appdata-directory ()
  (concatenate 'string (sb-posix:getenv "APPDATA") (dir-ending)))

(defun config-directory (appname &key (hidden t))
  #-:windows (merge-pathnames (concatenate 'string (when hidden ".") appname (dir-ending)) (home-directory))
  #+:windows (merge-pathnames appname (appdata-directory)))

(defun config-file (appname filename)
  (merge-pathnames (config-directory appname) filename))

(defmacro define-config-dir (fun-name appname)
  `(defun ,fun-name () (config-directory ,appname)))

(defmacro define-config-file (fun-name appname filename)
  `(defun ,fun-name () (config-file ,appname ,filename)))

(defun mkdir-if-not-exists (dir)
  (when (not (uiop:directory-exists-p dir))
    (sb-posix:mkdir dir #o0777)))

(defmacro with-write-config (str conf_file &body body)
  (format t "args:~S~%" str)
  (let ((conf conf_file))
    `(with-open-file (,str ,conf
                           :if-does-not-exist :create
                           :if-exists :supersede
                           :direction :output)
       ,@body)))
