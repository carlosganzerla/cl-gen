(defpackage #:examples (:use #:cl #:cl-gen))
(in-package #:examples)

#+sbcl
(require :sb-sprof)

(declaim (optimize speed))

;;;; This is a more realistic example. This program will lookup a string on
;;;; a file. If it exist, it'll return T and the position that the string
;;;; begins. Other wise, returns nil twice.


;; Generic generator that streams position and character
(defun file-char-generator (path)
  (with-open-file (stream path :direction :input)
    (loop for c = (read-char stream nil :eof) then (read-char stream nil :eof)
          for pos = 1 then (1+ pos)
          until (eql c :eof)
          do (yield c pos))))

;; Function that checks for a match.
(defun match-checker (path word)
  (let* ((word-length (length word))
         (match (make-string word-length))
         (match-index 0))
    (generator-bind (c pos) ((file-char-generator path) (values nil nil))
      (when (char= c (char word match-index))
        (setf (char match match-index) c)
        (incf match-index)
        (when (= word-length match-index) (stop t (- pos word-length)))
        (next))
      (setf match-index 0))))

(match-checker #p"src/cl-gen.lisp" "yield")
(match-checker #p"src/examples.lisp" "albert einstein")
(match-checker #p"src/examples.lisp" "marcus aurelius")
(match-checker #p"src/string-finder.lisp" "i'm always found")


;; Profiling with huge file
#+sbcl
(progn
  (sb-profile:profile match-checker)
  (unwind-protect (prog1
                    (dotimes (x 10)
                      (match-checker #p"~/Documents/Batting.csv" "willima08"))
                    (sb-profile:report))
    (sb-profile:unprofile match-checker)))
