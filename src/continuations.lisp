(in-package #:cl-gen)

(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro with-cc-context (&body body)
  (with-gensyms (block)
    `(block ,block
            (let (($cc #'identity)
                  ($stop ,(return-lambda block)))
              ,@body))))

(defuncont stop (&rest args)
  (apply $stop args))

(defuncont cc (&rest args)
  (apply $cc args))

(defmacro defuncont (name args &body body)
  (let ((f (intern (concat "%" name))))
    `(progn
       (defmacro ,name (&rest args)
         `(,',f $cc $stop ,@args))
       (defun ,f ($cc $stop ,@args) 
         (declare (ignorable $cc $stop))
         ,@body))))

(defmacro cc-bind (bindings form &body body)
  (with-gensyms (block)
    `(let (($cc (lambda ,bindings 
                  (block ,block 
                         (let (($stop ,(return-lambda block)))
                           (declare (ignorable $stop))
                           ,@body)))))
       (declare (ignorable $cc))
       ,form)))

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
