(in-package #:cl-gen)

(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro with-continuation-context (&body body)
  (with-gensyms (block)
    `(block ,block
            (let (($cc #'identity)
                  ($stop ,(return-lambda block)))
              ,@body))))

(defuncont return-cont (&rest args)
  (apply $stop args))

(defuncont continuation (&rest args)
  (apply $cc args))

(defmacro defuncont (name args &body body)
  (let ((f (intern (concat "%" name))))
    `(progn
       (defmacro ,name (&rest args)
         `(,',f $cc ,@args))
       (defun ,f ($cc ,@args) 
         (declare (ignorable $cc))
         ,@body))))

(defmacro continuation-bind (bindings form &body body)
  (with-gensyms (block)
    `(let (($cc (lambda ,bindings 
                  (block ,block 
                         (let (($stop ,(return-lambda block)))
                           ,@body)))))
       ,form)))

(defuncont printy (x)
  (progn (format t "Printy ~A~%" x) (continuation x)))

(defuncont prinky (x)
  (progn (format t "Prinky ~A~%" x) (continuation x)))

(defuncont prinpy (x)
  (progn (format t "Prinpy ~A~%" x) (continuation x)))


(defuncont bazzie (n)
  (continuation-bind (x) (progn (print $cc) (continuation 3))
    (print "evaled x")
    (continuation-bind (y) (prinky (1+ n))
      (print "evaled y")
      (continuation-bind (z) (prinpy (1- n))
        (print "evaled z")
        (format t "albierto tien ~A ~A ~A garrafitas~%" x y z)))))

(defuncont c ()
  (continuation-bind (x) (progn (print $cc) (continuation 3))
    (print 'eae)
    (print "evaled x")
    x))

(with-continuation-context (c))
(with-continuation-context (bazzie 5))
