(defpackage :apputils
  (:use :cl)
  (:export
    define-config-dir
    define-config-file
    define-storage-dir
    mkdir-if-not-exists
    with-write-config
    with-read-config
    dir-ending
    ))

(in-package :apputils)

(defun dir-ending ()
  (format nil "~c" (uiop:directory-separator-for-host)))

(defun home-directory ()
  #-:win32 (concatenate 'string (sb-posix:getenv "HOME") (dir-ending))
  #+:win32 (concatenate 'string (sb-posix:getenv "USER") (dir-ending)))

;#+:windows
;windows specific
(defun appdata-directory ()
  (concatenate 'string (sb-posix:getenv "APPDATA") (dir-ending)))

(defun config-directory (appname &key (hidden t))
  #-:win32 (merge-pathnames (concatenate 'string (when hidden ".") appname (dir-ending)) (home-directory))
  #+:win32 (merge-pathnames appname (appdata-directory)))

(defun config-file (appname filename)
  (merge-pathnames (config-directory appname) filename))

(defmacro define-config-dir (fun-name appname)
  `(defun ,fun-name () (config-directory ,appname)))

(defmacro define-config-file (fun-name appname filename)
  `(defun ,fun-name () (config-file ,appname ,filename)))

(defmacro define-storage-dir (fun-name appname storagedirname)
  `(defun ,fun-name () (config-directory (concatenate 'string ,appname (dir-ending) ,storagedirname (dir-ending)))))

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

(defmacro with-read-config (str conf_file &body body)
  (format t "args:~S~%" str)
  (let ((conf conf_file))
    `(with-open-file (,str ,conf :direction :input)
       ,@body)))
