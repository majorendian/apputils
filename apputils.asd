(defpackage apputils-system
  (:use :cl :asdf))

(in-package :apputils-system)
(defsystem "apputils"
  :description "Various usefull utilities when creating applications" 
  :version "1.0.0"
  :author "Ernest Deák <gordon.zar@gmail.com" 
  :license "BSD 2-Clause License"
  :components ((:file "apputils")))
