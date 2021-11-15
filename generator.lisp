(defvar *stop* (lambda ()))

(defvar *restarts* nil)

(defmacro stop (&optional x)
  `(funcall *stop* ,x))

(defmacro stop-when (test-from &body body)
  (with-gensyms (body-eval)
    `(when ,test-from
      (let ((,body-eval (progn ,@body)))
        (stop ,body-eval)))))

(defmacro stop-unless (test-from &body body)
  (with-gensyms (body-eval)
    `(unless ,test-from
      (let ((,body-eval (progn ,@body)))
        (stop ,body-eval)))))

(defmacro yield (form)
  (with-gensyms (value)
    `(let ((,value ,form))
       (if (car *restarts*)
           (invoke-restart (car *restarts*) ,value (cdr *restarts*))
           (error "Cannot yield value outside of a generator context"))
       ,value)))


(defmacro generator-consume ((binding generator-form) &body body) 
  (with-gensyms (block restart)
    `(block ,block
            (restart-bind 
              ((,restart (lambda (,binding *restarts*)
                           (let ((*stop* (lambda (x) 
                                           (return-from ,block x))))
                             ,@body))))
              (let ((*restarts* (cons ',restart *restarts*))) 
                ,generator-form)))))
