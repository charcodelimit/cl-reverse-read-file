;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-

(in-package :cl-user)

(defpackage #:cl-reverse-read-file-test
  (:use :cl :cl-reverse-read-file :fiveam))
(in-package #:cl-reverse-read-file-test)

(def-suite :cl-reverse-read-file-test :description "test suite")

(in-suite :cl-reverse-read-file-test)

(defmacro with-test-file-at-end ((stream pathname) &body body)
  `(with-open-file (,stream ,pathname
                            :direction :input
                            #-abcl :element-type #-abcl :default
                            :external-format :default)
     (let ((file-size (file-length str)))
       (file-position ,stream file-size)
       ,@body)))

(def-test example-test (:suite :cl-reverse-read-file-test)
  (with-test-file-at-end (str #P"test-1.dat")
    (is (= file-size (file-position str) file-size))
    ;; read lines
    ))
