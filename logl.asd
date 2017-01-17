;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:logl-asd
  (:use :cl :asdf))

(in-package :logl-asd)

(defsystem logl
  :name "logl"
  :version "0.0.0"
  :maintainer "fouric"
  :author "fouric"
  :license "All rights reserved"
  :description "DESCRIPTION HERE"

  :serial t
  :depends-on (:sdl2 :cl-opengl :fouriclib)
  :pathname "src"
  :components ((:file "logl")))
