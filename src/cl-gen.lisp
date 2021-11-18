(in-package #:cl-gen)

(defmacro with-gensyms (syms &body body)
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) syms))
     ,@body))

(defun mklist (obj)
  (if (consp obj)
      obj
      (list obj)))

(defun out-of-context-error ()
  (error "Cannot call yield or stop outside of a generator context"))

(defvar *stop* (lambda (&rest x)
                 (declare (ignore x))
                 (out-of-context-error)))

(defvar *next* (lambda (&rest x)
                 (declare (ignore x))
                 (out-of-context-error)))

(defvar *restarts* nil)

(defun stop (&rest x)
  (apply *stop* x))

(defun next (&rest x)
  (apply *next* x))

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

(defmacro generator-bind (bindings
                          (generator
                            &optional (return-form nil return-form-supplied-p))
                          &body body)
  (with-gensyms (whole-block restart lambda-block)
    `(block ,whole-block
            (restart-bind
              ((,restart (lambda (*restarts* ,@bindings)
                           (block
                             ,lambda-block
                             (let ((*stop*
                                     (lambda (&rest x)
                                       (return-from ,whole-block
                                                    (apply #'values x))))
                                   (*next*
                                     (lambda (&rest x)
                                       (return-from ,lambda-block
                                                    (apply #'values x)))))
                               ,@body)))))
              (let ((*restarts* (cons ',restart *restarts*)))
                ,@(if return-form-supplied-p
                      `(,generator ,return-form)
                      `(,generator)))))))

(defmacro generator-collect (bindings generator-form &body body)
  (with-gensyms (lst)
    `(let (,lst)
       (generator-bind ,bindings (,generator-form)
         (push (progn ,@body) ,lst))
       (nreverse ,lst))))
