(defpackage #:examples (:use #:cl #:cl-gen))
(in-package #:examples)

#+sbcl
(require :sb-sprof)

(declaim (optimize speed))

;;;; This is a more realistic example. This program will lookup a string on
;;;; a file. If it exist, it'll return T and the position that the string
;;;; begins. Other wise, returns nil twice.


;; Generic generator that streams position and character
(defun char-generator (stream)
  (loop for c = (read-char stream nil :eof) then (read-char stream nil :eof)
        for pos = 1 then (1+ pos)
        until (eql c :eof)
        do (yield c pos)))

;; Function that checks for a match.
(defun match-checker (stream word)
  (let* ((word-length (length word))
         (match (make-string word-length))
         (match-index 0))
    (generator-bind (c pos) ((char-generator stream) (values nil nil))
      (when (char= c (char word match-index))
        (setf (char match match-index) c)
        (incf match-index)
        (when (= word-length match-index) (stop t (- pos word-length)))
        (next))
      (setf match-index 0))))

(defun file-match-checker (path word)
  (with-open-file (stream path :direction :input) 
    (match-checker stream word)))

(file-match-checker #p"src/cl-gen.lisp" "yield")
(file-match-checker #p"src/examples.lisp" "albert einstein")
(file-match-checker #p"src/examples.lisp" "marcus aurelius")
(file-match-checker #p"src/string-finder.lisp" "i'm always found")


;; Profiling with huge file
#+nil
(progn
  (sb-profile:profile file-match-checker)
  (unwind-protect (prog1
                    (dotimes (x 10)
                      (file-match-checker 
                        #p"~/Documents/Batting.csv" 
                        "willima08"))
                    (sb-profile:report))
    (sb-profile:unprofile file-match-checker)))
