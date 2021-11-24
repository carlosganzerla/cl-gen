(defun next (&rest args)
  (if (functionp $next)
      (multiple-value-bind (next result) (apply $next args)
        (setf $next next)
        (values result t))
      (values nil nil)))


(defmacro yield-bind (bindings form &body body)
  `(continuation-bind ,bindings *continuation*
     (values (progn ,@body) ,form)))

(defmacro defun* (name bindings &body body)
  `(defun ,name ,bindings
     (continuation-bind () () ,@body)))

(defmacro generator-context (form &body body)
  `(let (($next ,form))
     ,@body))
