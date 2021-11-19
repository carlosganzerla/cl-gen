(in-package #:cl-gen)

(defvar *restarts* nil)

(defun out-of-context-error ()
  (error "Cannot call yield or stop outside of a generator context"))

(defvar *next* (list (lambda (&rest x) 
                       (declare (ignore x)) 
                       (out-of-context-error))))

(defvar *stop* (list (lambda (&rest x) 
                       (declare (ignore x)) 
                       (out-of-context-error))))

(defun stop (&rest args)
  (apply (car *stop*) args))

(defun next (&rest args)
  (apply (car *next*) args))

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

(defun yield (&rest values)
  (if (car *restarts*)
      (apply #'invoke-restart (car *restarts*) (cdr *restarts*) values)
      (out-of-context-error)))

(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro generator-bind (bindings
                          (generator &optional (return-form 
                                                 nil return-form-supplied-p))
                          &body body)
  (with-gensyms (whole-block restart lambda-block)
    `(block ,whole-block
            (restart-bind
              ((,restart 
                 (lambda (*restarts* ,@bindings)
                   (block ,lambda-block
                          (cons-let ((*stop* ,(return-lambda whole-block))
                                     (*next* ,(return-lambda lambda-block)))
                            ,@body)))))
              (cons-let ((*restarts* ',restart))
                ,@(if return-form-supplied-p
                      `(,generator ,return-form)
                      `(,generator)))))))

(defmacro generator-collect (bindings generator-form &body body)
  (with-gensyms (lst)
    `(let (,lst)
       (generator-bind ,bindings (,generator-form)
         (push (progn ,@body) ,lst))
       (nreverse ,lst))))
