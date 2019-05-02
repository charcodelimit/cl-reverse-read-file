;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-

(in-package :cl-user)

(defpackage #:cl-reverse-read-file
  (:use :cl)
  (:export #:reverse-file
           #:reverse-read-line))
(in-package #:cl-reverse-read-file)

(defun reverse-file (pathname)
  "COLLECTs lines in file named PATHNAME starting from the end of the file"
  (with-open-file (str pathname
                       :direction :input
                       :element-type :default
                       :external-format :default)
    (let ((file-size (file-length str)))
      (file-position str file-size)
      ;;; read lines from stream and collect into a list
      )))
