(in-package #:cl-gen)

(defvar *ignored* (gensym))

(defun multi-id (&rest args)
  (apply #'values args))

(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro cc-context (&body body)
  (with-gensyms (block)
    `(block ,block
            (let (($cc #'multi-id) ($stop ,(return-lambda block)))
              (declare (ignorable $stop $cc))
              ,@body))))

(defmacro defuncc (name args &body body)
  (let ((f (intern (concat "%" name))))
    `(progn
       (defmacro ,name (&rest args)
         `(,',f $cc $stop ,@args))
       (defun ,f ($cc $stop ,@args)
         (declare (ignorable $cc $stop))
         ,@body))))

(defuncc stop (&rest args)
  (apply $stop args))

(defuncc cc (&rest args)
  (apply $cc args))

(defmacro %cc-bind (bindings form declaration &body body)
  (with-gensyms (block)
    `(let (($cc (lambda ,bindings
                  ,declaration
                  (block ,block
                         (let (($stop ,(return-lambda block)))
                           (declare (ignorable $stop))
                           ,@body)))))
       (declare (ignorable $cc))
       ,form)))

(defmacro cc-bind (bindings form &body body)
  `(%cc-bind ,bindings ,form nil ,@body))
