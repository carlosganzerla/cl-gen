(in-package #:cl-gen)

(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro cc-context (&body body)
  (with-gensyms (block)
    `(block ,block
            (let (($cc #'identity) ($stop ,(return-lambda block)))
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


(defmacro cc-bind (bindings form &body body)
  (with-gensyms (block)
    `(let (($cc (lambda ,bindings 
                  (block ,block 
                         (let (($stop ,(return-lambda block)))
                           (declare (ignorable $stop))
                           ,@body)))))
       (declare (ignorable $cc))
       ,form)))

(defstruct generator (call nil :type function))

(defun %next (gen &rest args)
  (when (generator-p gen)
    (with-slots (call) gen
      (apply call args))))

(defmacro yield-bind (form bindings &body body)
  `(cc-bind ,bindings (values (make-generator :call $cc) ,form)
     ,@body))

(defmacro defgen (name bindings &body body)
  `(defuncc ,name ,bindings
     (yield-bind () () ,@body)))

(defmacro next-bind ((var-bind gen-bind) (gen-form &rest values) 
                     &body body)
  `(multiple-value-bind (,gen-bind ,var-bind) (funcall #'%next 
                                                       ,gen-form
                                                       ,@values)
     ,@body))

(defmacro generator-context (generator &body body)
  `(let (($generator ,generator))
     (declare (ignorable $generator))
     ,@body))

(defmacro next ((&rest values) &body body)
  `(next-bind (current $generator) ($generator ,@values)
     (declare (ignorable $generator current))
     ,@body))
