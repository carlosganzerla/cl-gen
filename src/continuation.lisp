(in-package #:cl-gen)

(defstruct generator cont)

(defvar *continuation* (list #'identity))
(defvar *stop* (list #'identity))

(defun stop (&rest args)
  (apply (car (last *stop* 2)) args))

(defun return-cont (&rest args)
  (apply (car *stop*) args))

(defun next (&rest args)
  (apply *next* args))

(defun continuation (&rest args)
  (apply (car *continuation*) args))

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
                  (lambda ,bindings 
                    (block ,block 
                           (cons-let ((*stop* ,(return-lambda block)))
                             ,@body)))))
       ,form)))



(defmacro later-bind (bindings &body body)
  `(continuation-bind ,bindings (car *continuation*)
     ,@body))

(defmacro defun* (name bindings &body body)
  (with-gensyms (next-fn)
    `(progn 
      (defun ,name ,bindings
        (let* ((,next-fn (lambda (&rest args)
                           (if (functionp ,next-fn)
                               (let ((result (next args)))
                                 (setf ,next-fn result)
                                 (values result t))
                               
               (*next* ,next-fn))
          (lambda () ,@body)))))))))


(defun printy (x)
  (progn (format t "Printy ~A~%" x) (continuation x)))

(defun prinky (x)
  (progn (format t "Prinky ~A~%" x) (continuation x)))

(defun prinpy (x)
  (progn (format t "Prinpy ~A~%" x) (continuation x)))

(defuncont baz ()
  (continuation-bind (x) (car *continuation*)
    (continuation-bind (y) (car *continuation*)
      (continuation-bind (z) (car *continuation*)
        (format t "albierto tien ~A ~A ~A garrafitas~%" x y z)))))

(defun lulz ()
  (do ((x 0 (1+ x))
       (fn (baz) (funcall fn x)))
      ((not (functionp fn)) fn)
      (when (> x 0) (return fn))))

(defun* bar ()
  (print "oi"))
