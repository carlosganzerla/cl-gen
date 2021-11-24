(in-package #:cl-gen)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

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

(defun clean-lambda-list (lambda-list)
  (remove-if (lambda (param)
               (and (symbolp param) (char= #\&(char (symbol-name param) 0))))
             lambda-list))
