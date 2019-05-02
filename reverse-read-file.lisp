;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        reverse-read-file.lisp
;;; LANGUAGE:    Common-Lisp
;;;
;;; DESCRIPTION
;;;
;;;
;;;
;;; Author: Christian Hofmann-Fuchs
;;;
;;; Created: Fr Mai  3 07:27:34 2019 (+0200)
;;;
;;; Last-Updated: Mi Mai  8 07:37:30 2019 (+0200)
;;;           By: Christian Hofmann-Fuchs
;;;           Update #: 63
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

(defpackage #:cl-reverse-read-file
  (:use :cl)
  (:export #:reverse-file
           #:do-reverse-read-line))
(in-package #:cl-reverse-read-file)

(defun reverse-file (pathname)
  "COLLECTs lines in file named PATHNAME starting from the end of the file"
  (with-open-file (str pathname
                       :direction :input
                       :element-type :default
                       :external-format :default)
    (prog ((file-size (file-length str))
           (lines))
      (file-position str file-size)
      (do-reverse-read-line str
        (lambda (line)
          (if line
              (push line lines)
              (return (nreverse lines))))))))

(defun do-reverse-read-line (stream callback &key (buffer-size 8192))
  "CALLs closure CALLBACK with the current line while traversing the file-stream
STREAM from file-end to file-start.
Data is read in chunks of size BUFFER-SIZE"
  (declare (type file-stream stream))
  (let ((buffer (make-array buffer-size :element-type 'character))
        (remaining (file-position stream))
        (line nil))
    (when (> remaining 0)
      (file-position stream (- remaining 1))
      (when (eql #\newline (peek-char nil stream))
        ;; skip first newline character
        (decf remaining))
      ;; loop through file
      (loop :while (> remaining 0)
            :with pos = nil
            :with current-line = nil
            :for size = (min remaining buffer-size)
              :then (min remaining buffer-size)
            :do (file-position stream (- remaining size))
                (read-sequence buffer stream :start 0 :end size)
                (decf remaining size)
                ;; loop through buffer
                (loop :while (> size 0)
                      :do (setq pos
                                (position #\newline buffer :end size :from-end t))
                          (cond (pos ;; newline found
                                 (file-position stream (+ remaining pos))
                                 (setq current-line
                                       (concatenate
                                        'string
                                        (subseq buffer (+ pos 1) size) line))
                                 (setq line nil)
                                 (setq size pos)
                                 (funcall callback current-line))
                                (t ;; no newline
                                 (setq line
                                       (concatenate 'string (subseq buffer 0 size) line))
                                 (file-position stream remaining)
                                 (setq size 0)))))
      (funcall callback line))))
