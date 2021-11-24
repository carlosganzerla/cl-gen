(in-package #:cl-gen)

(defun printy (x)
  (progn (format t "Printy ~A~%" x) (continuation x)))

(defun prinky (x)
  (progn (format t "Prinky ~A~%" x) (continuation x)))

(defun prinpy (x)
  (progn (format t "Prinpy ~A~%" x) (continuation x)))

(defun* baz ()
  (yield-bind (x) (print "evaled x")
    (yield-bind (y) (print "evaled y")
      (yield-bind (z) (print "evaled z")
        (format t "albierto tien ~A ~A ~A garrafitas~%" x y z)))))


(defun lulz ()
  (generator-context (baz)
    (next)
    (next 2)))

(defuncont bazzie (n)
  (continuation-bind (x) (printy n)
    (print "evaled x")
    (continuation-bind (y) (prinky (1+ n))
      (print "evaled y")
      (continuation-bind (z) (prinpy (1- n))
        (print "evaled z")
        (format t "albierto tien ~A ~A ~A garrafitas~%" x y z)))))

(defun* bar ()
  (print "oi"))
