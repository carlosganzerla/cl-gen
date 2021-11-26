(in-package #:cl-gen)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defmacro aif (test-form if-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,if-form ,else-form)))

(defmacro awhen (form &body body)
  `(aif ,form ,@body))

(defun mklist (obj)
  (if (consp obj)
      obj
      (list obj)))

(defmacro cons-let (bindings &body body)
  `(let ,(mapcar (lambda (binding)
                   `(,(car binding) (cons ,@(cdr binding) ,(car binding))))
                 bindings)
     ,@body))

(defun concat (&rest args)
  (apply #'concatenate 'string (mapcar (lambda (arg)
                                         (if (symbolp arg)
                                             (symbol-name arg)
                                             arg))
                                       args)))
