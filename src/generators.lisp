(in-package #:cl-gen)

(defstruct generator (call nil :type function))

(defun %next (gen &rest args)
  (when (generator-p gen)
    (with-slots (call) gen
      (apply call args))))

(defmacro yield-bind (bindings form &body body)
  (with-gensyms (rest)
    `(%cc-bind (&optional ,@bindings &rest ,rest) 
               (multiple-value-call #'values (make-generator :call $cc) ,form)
               (declare (ignore ,rest))
               ,@body)))

(defmacro defgen (name bindings &body body)
  `(defuncc ,name ,bindings
     (yield-bind () () ,@body)))

(defmacro next-bind (var-binds (gen-binding &rest args) &body body)
  `(multiple-value-bind (,gen-binding ,@var-binds) (funcall #'%next 
                                                            ,gen-binding 
                                                            ,@args)
     (declare (ignorable ,gen-binding))
     ,@body))

(defmacro start-let (bindings &body body)
  `(let ,bindings
     ,@(reduce 
         (lambda (e acc)
           `((next-bind () (,e)
               ,@acc))) 
         (mapcar #'car bindings)
         :from-end t
         :initial-value body)))

(defmacro %generator-bind ((var-binds rec-bind gen-bind) gen-form &body body)
  `(labels ((,rec-bind (,gen-bind)
              (next-bind ,var-binds (,gen-bind ()) 
                (when ,gen-bind
                  ,@body))))
     (,rec-bind ,gen-form)))

(defmacro generator-bind (var-binds gen-form &body body)
  (with-gensyms (rec-bind gen-bind)
    `(%generator-bind (,var-binds ,rec-bind ,gen-bind) ,gen-form 
       (labels ((next ()
                  (,rec-bind ,gen-bind)))
         ,@body))))

(defmacro generator-loop (var-binds gen-form &body body)
  (with-gensyms (rec-bind gen-bind)
    `(%generator-bind (,var-binds ,rec-bind ,gen-bind) 
                      ,gen-form 
                      (progn ,@body (,rec-bind ,gen-bind)))))

(defmacro generator-do (varlist endlist &body body)
  (let ((rec (gensym))
        (varlist (mapcar (lambda (var)
                           (destructuring-bind 
                             (bind init &optional (step nil step-p)) var
                             (declare (ignore step))
                             (if step-p
                                 var
                                 `(,bind ,init ,bind))))
                         varlist)))
    `(labels ((,rec ,(mapcar #'car varlist)
                (if ,(car endlist)
                    ,(cadr endlist)
                    (yield-bind () (progn ,@body)
                      (,rec ,@(mapcar #'caddr varlist))))))
       (,rec ,@(mapcar #'cadr varlist)))))
