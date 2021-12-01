(in-package #:cl-gen)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defun concat (&rest args)
  (apply #'concatenate 'string (mapcar (lambda (arg)
                                         (if (symbolp arg)
                                             (symbol-name arg)
                                             arg))
                                       args)))
