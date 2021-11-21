(in-package #:cl-gen)

(defun out-of-context-error ()
  (error "Cannot call yield or stop outside of a generator context"))

(defvar *next* (list (lambda (&rest x)
                       (declare (ignore x))
                       (out-of-context-error))))

(defvar *stop* (list (lambda (&rest x)
                       (declare (ignore x))
                       (out-of-context-error))))

(defvar *yield* (list (lambda (&rest x)
                        (declare (ignore x))
                        (out-of-context-error))))

(defun stop (&rest args)
  (apply (car *stop*) args))

(defun next (&rest args)
  (apply (car *next*) args))

(defun yield (&rest args)
  (apply (car *yield*) (cdr *yield*) args))

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


(defun return-lambda (block-name)
  `(lambda (&rest x)
     (return-from ,block-name (apply #'values x))))

(defmacro generator-bind (bindings
                          (generator &optional (return-form
                                                 nil return-form-supplied-p))
                          &body body)
  (with-gensyms (whole-block lambda-block)
    `(block ,whole-block
            (cons-let 
              ((*yield* 
                 (lambda (*yield* ,@bindings)
                   (block ,lambda-block
                          (cons-let ((*stop* ,(return-lambda whole-block))
                                     (*next* ,(return-lambda lambda-block)))
                            ,@body)))))
              ,@(if return-form-supplied-p
                    `(,generator ,return-form)
                    `(,generator))))))

(defmacro generator-collect (bindings generator-form &body body)
  (with-gensyms (lst)
    `(let (,lst)
       (generator-bind ,bindings (,generator-form)
         (push (progn ,@body) ,lst))
       (nreverse ,lst))))
