(in-package #:cl-gen)

(defuncont printy (x)
  (progn (format t "Printy ~A~%" x) (cc x)))

(defuncont prinky (x)
  (progn (format t "Prinky ~A~%" x) (cc x)))

(defuncont prinpy (x)
  (progn (format t "Prinpy ~A~%" x) (cc x)))


(defuncont bazzie (n)
  (cc-bind (x) (progn (print $cc) (cc 3))
    (print "evaled x")
    (cc-bind (y) (prinky (1+ n))
      (print "evaled y")
      (cc-bind (z) (prinpy (1- n))
        (print "evaled z")
        (format t "albierto tien ~A ~A ~A garrafitas~%" x y z))
      (cc 5))))

(defuncont c ()
  (cc-bind (x) (bazzie 5)
    (print 'eae)
    (print "evaled x")
    x))

(with-cc-context (c))

(defuncont boris ()
  (cc-bind (x) $cc
    (format t "evaled x ~A~%" x)
    (cc-bind (y) $cc
      (format t "evaled y ~A~%" y)
      (stop 33)
      (cc-bind (z) $cc
        (format t "evaled z ~A~%" z)
        (print $cc)
        (format t "gameover son~%")))))

(with-cc-context (boris))


(defun dft (tree)
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
         (dft (cdr tree)))))

(setq *saved* nil)

(defuncont dft-node (tree)
  (cond ((null tree) (restartx))
        ((atom tree) (cc tree))
        (t (push (lambda () (dft-node (cdr tree)))
                 *saved*)
           (dft-node (car tree)))))

(defuncont restartx ()
  (if *saved*
      (funcall (pop *saved*))
      (cc 'done)))

(defuncont dft2 (tree)
  (setq *saved* nil)
  (cc-bind (node) (dft-node tree)
    (cond ((eq node 'done) (cc nil))
          (t (princ node)
           (restartx)))))

(setq t1 '(a (b (d h)) (c e (f i) g))
      t2 '(1 (2 (3 6 7) 4 5)))

(with-cc-context (dft2 t1))
