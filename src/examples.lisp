(defpackage #:examples
  (:use #:cl #:cl-gen))
(in-package #:examples)

(defgen generator ()
  (yield-bind () "Lorem"
    (yield-bind () "Ipsum"
      (yield-bind () "Dolor"))))

(cc-context
  (let ((gen (generator)))
    (next-bind (x) (gen)
      (print x)
      (next-bind (y) (gen)
        (print y)
        (next-bind (z) (gen)
          (print z)
          (print (concatenate 'string x y z)))
        ;; May be called again on a previous point
        (next-bind (y) (gen)
          (print y)
          (next-bind (z) (gen)
            ;; Returns last form
            t))))))

(defgen your-name ()
  (yield-bind (first-name) ()
    (yield-bind (second-name) ()
      (format t "Hello, ~A ~A!~%" first-name second-name))))

(cc-context
  (start-let ((gen (your-name)))
    (next-bind () (gen (read-line))
      (next-bind () (gen (read-line))))))
