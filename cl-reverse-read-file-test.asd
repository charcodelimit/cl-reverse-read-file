;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        cl-reverse-read-file-test.asd
;;; LANGUAGE:    Common-Lisp
;;;
;;; DESCRIPTION
;;;  System Definition
;;;
;;;
;;; Author: Christian Hofmann-Fuchs
;;;
;;; Created: Fr Mai 17 18:24:02 2019 (+0200)
;;;
;;; Last-Updated: So Mai 19 10:38:11 2019 (+0200)
;;;           By: Christian Hofmann-Fuchs
;;;           Update #: 2
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

(defpackage :cl-read-file-test-package (:use #:cl #:asdf))
(in-package :cl-read-file-test-package)

(defsystem :cl-reverse-read-file-test
  :author "Christian Hofmann-Fuchs"
  :description "test suite"
  :license "BSD"
  :depends-on (:fiveam :cl-reverse-read-file)
  :components ((:file "reverse-read-file-test"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam :run! :cl-reverse-read-file-test)))
