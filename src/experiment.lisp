(in-package #:cl-gen)

(defstruct generator call)

(defun next (gen)
  (when (generator-call gen)
    (let ((next (funcall generator-call gen)))
       (setf (generator-call gen) next)
       
       )))

(defmacro continuation-bind (calls bindings &body body)
  (let ((f `(lambda ,bindings ,@body))) 
    `(progn (push ,f ,calls) ,f)))

(defmacro defun* (name params &body body)
  `(defun ,name ,params
     ,(let ((gen-body (reduce (lambda (form acc)
                                (if (equal 'cont (car form))
                                    `(lambda () ,@acc)
                                    `(,form ,acc)))
                              body
                              :initial-value nil
                              :from-end t)))
        `(make-generator :call (lambda () ,@gen-body)))))

(defun* g ()
  (print 4)
  (cont 4)
  (print 2)
  (cont 2))

(defun* a ()
  (print 1)
  (cont 1)
  (print 2))

(let ((gen a))
  (funcall next))

(let ((g (g)))
  (next g)
  (next g)
  (next g))

(continuation-bind x (a b c) (print a))
