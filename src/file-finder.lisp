(defpackage #:examples (:use #:cl #:cl-gen))
(in-package #:examples)

(defun file-char-generator (path)
  (with-open-file (stream path :direction :input)
    (loop for c = (read-char stream nil :eof) 
          then (read-char stream nil :eof)
          until (eql c :eof)
          do (yield c))))

(defun word-index-check (word path)
  (let ((match-index 0))
   (generator-bind (c) ((file-char-generator #p"src/package.lisp"))
     (if (char= x (char word match-index))
         (setf match-index (yield c (incf match-index)))
         (setf match-index 0)))))


