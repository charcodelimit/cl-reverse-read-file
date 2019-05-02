;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        reverse-read-file-test.lisp
;;; LANGUAGE:    Common-Lisp
;;;
;;; DESCRIPTION
;;;  test suite
;;;
;;;
;;; Author: Christian Hofmann-Fuchs
;;;
;;; Created: Di Apr 30 19:04:31 2019 (+0200)
;;;
;;; Last-Updated: Do Mai  9 21:16:51 2019 (+0200)
;;;           By: Christian Hofmann-Fuchs
;;;           Update #: 34
;;;
;;; Copyright (C) 2019, Christian Hofmann-Fuchs. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;;****************************************************************************

(in-package :cl-user)

(defpackage #:cl-reverse-read-file-test
  (:use :cl :cl-reverse-read-file :fiveam))
(in-package #:cl-reverse-read-file-test)

(def-suite :cl-reverse-read-file-test :description "test suite")

(in-suite :cl-reverse-read-file-test)

(defmacro reverse-read-line (str expected-string expected-pos buffer-size)
  `(prog ()
     (do-reverse-read-line ,str
       (lambda (line)
         (is (equal ,expected-string line))
         (is (= ,expected-pos (file-position ,str)))
         (return))
       :buffer-size ,buffer-size)))

(defun assert-read-line (str buffer-size)
  (reverse-read-line str "abc" 10 buffer-size)
  (reverse-read-line str "test" 5 buffer-size)
  (reverse-read-line str "3" 3 buffer-size)
  (reverse-read-line str "2" 1 buffer-size)
  (reverse-read-line str "1" 0 buffer-size))

(defmacro with-test-file-at-end ((stream pathname) &body body)
  `(with-open-file (,stream ,pathname
                            :direction :input
                            :element-type :default
                            :external-format :default)
     (let ((file-size (file-length str)))
       (file-position ,stream file-size)
       ,@body)))

(def-test test-reverse-read-line-last-line-linebreak ()
  (with-test-file-at-end (str #P"test-1.dat")
    (assert-read-line str 64)))

(def-test test-reverse-read-line-last-line-no-linebreak ()
  (with-test-file-at-end (str #P"test-2.dat")
    (assert-read-line str 64)))

(def-test test-reverse-read-line-only-last-linebreak ()
  (with-test-file-at-end (str #P"test-3.dat")
    (prog ()
      (do-reverse-read-line str
        (lambda (line)
          (is (equal "123testabc" line)))
        :buffer-size 64))))

(def-test test-reverse-read-line-no-linebreaks ()
  (with-test-file-at-end (str #P"test-4.dat")
    (prog ()
      (do-reverse-read-line str
        (lambda (line)
          (is (equal "123testabc" line)))
        :buffer-size 64))))

(def-test test-reverse-read-line-empty-file ()
  (with-test-file-at-end (str #P"test-5.dat")
    (prog ()
      (do-reverse-read-line str
        (lambda (line) (is (null line)))
        :buffer-size 64))))

(def-test test-reverse-read-line-minimum-buffer-size ()
  (with-test-file-at-end (str #P"test-1.dat")
    (assert-read-line str 1)))

(def-test test-reverse-read-line-with-buffer-refresh-required ()
  (with-test-file-at-end (str #P"test-1.dat")
    (assert-read-line str 8)))
