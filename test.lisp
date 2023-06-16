;;; TRYING OUT THE MUTUALLY RECURSIVE APPROACH


(defvar *block* (gensym))

(defvar *yield* (lambda (&rest args)
                 (declare (ignore args))
                 (error "Oops, this lib is not good :(")))


(defun yield (x)
  (funcall *yield* x))

(defmacro defgen (name args &body body)
  `(defun ,name ,args
     (lambda () ,@body)))

(defmacro generator-context (binding form &body body)
  `(block ,*block*
          (let ((*yield* (lambda (,binding)
                           (block nil
                                  ,@body
                                  (return-from ,*block*)))))
            (funcall ,form))))


(defmacro next-bind (val binding &body body)
  `(progn
     (setf *yield* (lambda (,binding)
                     (block nil 
                            ,@body
                            (return-from ,*block*)))) 
     (return ,val)))


(defgen test ()
  (dotimes (x 10)
    (yield x)
    (print "i'm dirty ass bitch")))

(generator-context y (test)
  (print y)
  (next-bind nil x (print x)
             (next-bind nil x (print x))
             ))

(next 5)
(next 5)



(defmacro next ()
  (lambda (progn ,@body ))
  )

(block c
       (block c
              (print "a")
              (return-from c))
       (print "b")
       )
(funcall (lambda ()
           (return 1)
           (print "x")
           ))

(defvar *t*)
(let ((*t* (lambda ()
             (setf *t* (lambda () (print "eae")))
             (funcall *t*))))
  (funcall *t*))
