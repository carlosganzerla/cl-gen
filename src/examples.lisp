(defpackage #:examples (:use #:cl #:cl-gen))
(in-package #:examples)

;;;; This file contains some example usage of cl-gen
;;;; Currently no actual unit tests exists, because I'm still trying to
;;;; build some examples that will make the proof of concept.

;;; Basic usage
(defun js-test ()
  (yield 1)
  (yield 2)
  (yield 3 5 6)
  ; you can yield multiple values, but the consumer must be ready for it
  (print "shouldnt call me!")
  (yield 4)
  (yield 5))

(generator-bind (x &rest stuff) ((js-test))
  (stop-when (= x 3) stuff)) ; Stop generating when X is 3, and return X

;; Sequential number generators

(defun numbers ()
  (do ((x 0 (1+ x)))
      (nil)
      (yield x)))

(defun even-numbers ()
  (generator-bind (x) ((numbers))
    (when (evenp x)
      (format t "Yield returns ~A~%" (yield x)))))

(generator-bind (x) ((even-numbers))
  (format t "Yield supplies ~A~%" x)
  (stop-when (>= x 100) 'ok)
  (- 100 x)) ; Yield returns the value of the body of GENERATOR-BIND

;;; List operations
(defun sum-generation (lst)
  (reduce (lambda (acc x) (yield (+ x acc))) lst))

;; The lambda body of SUM-GENERATION is "forwarded" to the body
(generator-bind (e) ((sum-generation '(1 2 3 4 5)))
  (print e))

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
  (generator-collect (e) (tree-generator tree)
    (if (funcall pred e)
        e
        (next))))

(find-tree #'evenp '(1 2 3 (5 4 99 0 3 (1 2) (1 (4 5))) 4 (1 2 (3) (1 3 2))))

;; Maps TREE onto FN and flats it
(defun tree-map (fn tree)
  (generator-collect (e) (tree-generator tree)
    (funcall fn e)))

;; For trolling purposes only
(tree-map (lambda (x) (intern (nreverse (symbol-name x))))
          '(as long as people use (common) lisp there will be (necessarily)
               wars (albert einstein (who we found out later that was
                                          actually lionel messi))))

;; Yield can return multiple values
(defun subsequent-elements (lst)
  (mapc #'yield  lst (cdr lst)))

;; You may change the return form of a generator
;;(the default is the return of the generator itself)
;; You may return multiple values from stop and the generator form also.
(defun sortedp (lst)
  (generator-bind (x y) ((subsequent-elements lst) (values t lst))
    (when (> x y)
      (stop nil (list x y)))))

(sortedp (list 1 0 3 4 5))
(sortedp (list 1 3 9 12 12 13))
(sortedp nil)

;; Next can also return multiple values
(defun weird-mapcan (lst)
  (mapcan (lambda (x)
            (multiple-value-bind (x y) (yield x)
              (list x y)))
          (copy-list lst)))

(generator-bind (x) ((weird-mapcan '(1 2 3 4 5)))
  ;; Same as values in this case
  (next x (format nil "~R" x)))
