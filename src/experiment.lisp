(in-package #:cl-gen)

(defstruct generator call)

(defun next (gen &rest values)
  (when (and gen (generator-call gen))
    (let ((next (apply (generator-call gen) values)))
      (when (functionp next)
        (return-from next (values (setf (generator-call gen) next) t)))))
  (values nil nil))


(defun generator-section (bindings &body body)
  `(lambda ,bindings
     ()
     )
  )

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
  (print 1)
  (print 1)
  (cont 4)
  (print 2)
  (print 4)
  (cont 2)
  (print 6)
  (print 7))

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
