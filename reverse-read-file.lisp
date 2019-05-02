;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        reverse-read-file.lisp
;;; LANGUAGE:    Common-Lisp
;;;
;;; DESCRIPTION
;;;  Implementation using a do-macro pattern
;;;
;;;
;;; Author: Christian Hofmann-Fuchs
;;;
;;; Created: Fr Mai  3 07:27:34 2019 (+0200)
;;;
;;; Last-Updated: So Mai 12 22:38:14 2019 (+0200)
;;;           By: Christian Hofmann-Fuchs
;;;           Update #: 112
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

(defmacro %inner-loop% (str buffer size remaining line body)
  "ITERATEs through BUFFER until the buffer-start is reached"
  (let ((begin-loop (make-symbol "begin-loop")))
    `(let ((pos 0))
       (tagbody
         ,begin-loop
         (when (> ,size 0)
           (setq pos (position #\newline ,buffer :end ,size :from-end T))
           (cond (pos
                  ;; newline found -> add buffer up to newline to the already
                  ;; read part of line, and execute body
                  (file-position ,str (+ ,remaining pos))
                  (setq ,line (concatenate 'string
                                           (subseq ,buffer (+ pos 1) ,size)
                                           ,line))
                  (setq ,size pos)
                  (progn ,@body)
                  (setq ,line nil))
                 (T
                  ;; no newline found -> add whole buffer to the already read
                  ;; part of line
                  (setq ,line
                        (concatenate 'string (subseq ,buffer 0 ,size) ,line))
                  (file-position ,str ,remaining)
                  (setq ,size 0)))
           (go ,begin-loop))))))

(defmacro do-reverse-read-line ((line stream &key (buffer-size 8192)) &body body)
  "TRAVERSEs the file-stream STREAM from file-end to file-start. BODY is executed
with the variable LINE bound to the current line.
Data is read in chunks of size BUFFER-SIZE"
  (let ((buffer (gensym "buffer"))
        (remaining (gensym "remaining")))
    `(prog ((,buffer (make-array ,buffer-size :element-type 'character))
            (,remaining (file-position ,stream))
            (,line nil))
       (declare (type file-stream ,stream)
                (type (or null string) ,line))
       (when (> ,remaining 0)
         (file-position ,stream (- ,remaining 1))
         (when (eql #\newline (peek-char nil ,stream))
           ;; skip first newline character
           (decf ,remaining))
         ;; iterate through file and fill buffer
         (loop :while (> ,remaining 0)
               :for size = (min ,remaining ,buffer-size)
                 :then (min ,remaining ,buffer-size)
               :do (file-position ,stream (- ,remaining size))
                   (read-sequence ,buffer ,stream :start 0 :end size)
                   (decf ,remaining size)
                   ;; iterate through buffer
                   (%inner-loop% ,stream ,buffer size ,remaining ,line ,body)
               :finally (when ,line (progn ,@body)))))))

(defun reverse-file (pathname)
  "COLLECTs lines in file named PATHNAME starting from the end of the file"
  (with-open-file (str pathname
                       :direction :input
                       :element-type :default
                       :external-format :default)
    (let ((file-size (file-length str))
          (lines))
      (file-position str file-size)
      (do-reverse-read-line (line str)
        (push line lines))
      (nreverse lines))))
