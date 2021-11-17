(defpackage #:examples (:use #:cl #:cl-gen))
(in-package #:examples)

;;;; This file contains some example usage of cl-gen
;;;; Currently no actual unit tests exists, because I'm still trying to
;;;; build some examples that will make the proof of concept.

;;; Basic usage
(defun js-test ()
  (yield 1)
  (yield 2)
  (yield 3)
  (print "shouldnt call me!")
  (yield 4)
  (yield 5))

(generator-bind (x (js-test))
  (stop-when (= x 3) x)) ; Stop generating when X is 3, and return X

;;; Sequential number generators

(defun numbers ()
  (do ((x 0 (1+ x))) 
      (nil)
      (yield x)))

(defun even-numbers ()
  (generator-bind (x (numbers))
    (when (evenp x)
      (format t "Yield returns ~A~%" (yield x)))))

(generator-bind (x (even-numbers))
    (format t "Yield supplies ~A~%" x)
    (stop-when (>= x 100) 'ok)
    (- 100 x)) ; Yield returns the value of the body of GENERATOR-BIND

;;; List operations
(defun sum-generation (lst)
  (reduce (lambda (acc x) (yield (+ x acc))) lst))

;; The lambda body of SUM-GENERATION is "forwarded" to the body
(generator-bind (e (sum-generation '(1 2 3 4 5)))
  (print e))

;;; Tree traversal

;; Generic tree traversal generator
(defun tree-generator (tree)
  (mapcar (lambda (x)
            (if (consp x)
                (tree-generator x)
                (yield x)))
          tree))

;; Generator collect will collect all results on a list unless you explicitly
;; skip an iteration
(defun find-tree (pred tree)
  (generator-collect (e (tree-generator tree))
    (if (funcall pred e)
        e
        (next))))

(find-tree #'evenp '(1 2 3 (5 4 99 0 3 (1 2) (1 (4 5))) 4 (1 2 (3) (1 3 2))))

;; Maps TREE onto FN and flats it
(defun tree-map (fn tree)
  (generator-collect (e (tree-generator tree))
    (funcall fn e)))

;; For trolling purposes only
(tree-map (lambda (x) (intern (nreverse (symbol-name x))))  
          '(as long as people use (common) lisp there will be (necessarily) 
               wars (albert einstein (who we found out later that was 
                                          actually lionel messi))))
