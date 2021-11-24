(in-package #:cl-gen)

(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro with-cc-context (&body body)
  (with-gensyms (block)
    `(block ,block
            (let (($cc #'identity) ($stop ,(return-lambda block)))
              (declare (ignorable $stop $cc))
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

(defstruct generator (call nil :type function))

(defun next (gen &rest args)
  (when (generator-p gen)
    (with-slots (call) gen
      (apply call args))))

(defmacro yield-bind (form bindings &body body)
  `(cc-bind ,bindings (values (make-generator :call $cc) ,form)
     ,@body))

(defmacro defun* (name bindings &body body)
  `(defuncont ,name ,bindings
     (yield-bind () () ,@body)))
