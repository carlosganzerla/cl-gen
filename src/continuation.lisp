(in-package #:cl-gen)

(defvar *continuation* (list #'identity))
(defvar *stop* (list #'identity))

(defun stop (&rest args)
  (apply (car (last *stop* 2)) args))

(defun return-cont (&rest args)
  (apply (car *stop*) args))

(defun continuation (&rest args)
  (apply (car *continuation*) (cdr *continuation*) args))

(defmacro clambda (bindings &body body)
  `(lambda (*continuation* ,@bindings)
     ,@body))

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
    `(cons-let ((*continuation* 
                  (clambda ,bindings 
                    (block ,block 
                           (cons-let ((*stop* ,(return-lambda block)))
                             ,@body)))))
       ,form)))


(defun printy (x)
  (progn (format t "Printy ~A~%" x) x))

(defun prinky (x)
  (progn (format t "Prinky ~A~%" x) x))

(defun prinpy (x)
  (progn (format t "Prinpy ~A~%" x) x))

(defuncont baz (n)
  (continuation-bind (x) (printy n)
    (continuation-bind (y) (prinky (1+ n))
      (continuation-bind (z) (prinpy (1- n))
        (stop (car *continuation*))
        (format t "albierto tien ~A ~A ~A garrafitas~%" x y z))
      (format t "vienna")
      (stop 22))))
