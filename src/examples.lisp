(defpackage #:examples
  (:use #:cl #:cl-gen))
(in-package #:examples)

(defuncc print-cc (&rest args)
  (cc (print args)))

(defuncc random-2 (max)
  (cc (random max) (random max)))

(defuncc hello-cc (max)
  (cc-bind (x y) (random-2 max)
    (print-cc x y)))

(cc-context (hello-cc 10))

(defuncc execute-later ()
  (print cl-gen::$cc) ; Don't do that
  (cc-bind (x) cl-gen::$cc
    (print cl-gen::$cc)
    (cc-bind (y z) (random-2 x)
      (print cl-gen::$cc)
      (print-cc x y z))))

(cc-context
  (let ((f (execute-later)))
    (funcall f 30)))

(defuncc lazy-loop ()
  (labels ((rec (x)
             (format t "Iterated ~A times :O~%" x)
             (cc-bind (&rest args) cl-gen::$cc
               (format t "Received ~A~%" args)
               (rec (1+ x)))))
    (rec 0)))

(cc-context
  (do ((x 0 (1+ x))
       (f (lazy-loop) (funcall f (random 15))))
      ((= 100 x))))

;; Generator basic usage

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
          (print z))))))

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
