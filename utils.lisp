(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defmacro aif (test-form if-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,if-form ,else-form)))

(defmacro awhen (form &body body)
  `(aif ,form ,@body))
