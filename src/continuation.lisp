(in-package #:cl-gen)

(setf *continuation* #'identity)
(setf *stop* (list #'identity))
(setf *next* nil)

(defun stop (&rest args)
  (apply (car (last *stop* 2)) args))

(defun return-cont (&rest args)
  (apply (car *stop*) args))

(defun next (&rest args)
  (if (functionp *next*)
      (multiple-value-bind (next result) (apply *next* args)
        (setf *next* next)
        (values result t))
      (values nil nil)))

(defun continuation (&rest args)
  (apply *continuation* args))

(defmacro defuncont (name args &body body)
  (let ((f (intern (concat "%" name))))
    `(progn
       (defmacro ,name ,args
         `(,',f *continuation* ,,@args))
       (defun ,f (*continuation* ,@args) ,@body))))

(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro continuation-bind (bindings form &body body)
  (with-gensyms (block)
    `(let ((*continuation* (lambda ,bindings 
                             (block ,block 
                                    (cons-let ((*stop* ,(return-lambda block)))
                                      ,@body)))))
       ,form)))

(defmacro yield-bind (bindings form &body body)
  `(continuation-bind ,bindings *continuation*
     (values (progn ,@body) ,form)))

(defmacro defun* (name bindings &body body)
  `(defun ,name ,bindings
     (continuation-bind () () ,@body)))

(defmacro generator-context (form &body body)
  `(let ((*next* ,form))
     ,@body))


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

(setf pau "cu")

(funcall (let ((pau 3))
           (lambda () (incf pau 4))))
