(in-package #:cl-gen)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defun out-of-context-error ()
  (error "Cannot call yield or stop outside of a generator context"))

(defvar *stop* (lambda (&optional x) 
                 (declare (ignore x))
                 (out-of-context-error)))

(defvar *next* (lambda (&optional x) 
                 (declare (ignore x))
                 (out-of-context-error)))

(defvar *restarts* nil)

(defun stop (&optional x)
  (funcall *stop* x))

(defun next (&optional x)
  (funcall *next* x))

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

(defun yield (value)
  (if (car *restarts*)
      (invoke-restart (car *restarts*) value (cdr *restarts*))
      (out-of-context-error)))

(defmacro generator-bind ((binding generator-form) &body body) 
  (with-gensyms (whole-block restart lambda-block)
    `(block ,whole-block
            (restart-bind 
              ((,restart (lambda (,binding *restarts*)
                           (block
                             ,lambda-block
                             (let ((*stop* (lambda (x) 
                                             (return-from ,whole-block x)))
                                   (*next* (lambda (x)
                                             (return-from ,lambda-block x)))) 
                               ,@body)))))
              (let ((*restarts* (cons ',restart *restarts*))) 
                ,generator-form)))))

(defmacro generator-collect ((binding generator-form) &body body)
  (with-gensyms (lst)
    `(let (,lst)
       (generator-bind (,binding ,generator-form)
         (push (progn ,@body) ,lst))
       (nreverse ,lst))))
