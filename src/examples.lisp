(in-package #:cl-gen)

(defgen generator ()
  (yield-bind () "Lorem"
    (yield-bind () "Ipsum"
      (yield-bind () "Dolor"))))

(cc-context
  (generator-bind (g) (generator)
    (print (next))
    (print (next))))

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


(defgen numbers ()
  (generator-do ((x 0 (+ 2 x)))
                (nil)
                x))

(defgen even-numbers ()
  (generator-bind (n) (numbers)
    (yield-bind () n
      (next))))

(cc-context
  (let ((gen (even-numbers)))
    (next-bind (x) (gen)
      (print x)
      (next-bind (x) (gen)
        (print x)))))

(cc-context
  (generator-bind (x) (even-numbers)
    (print x)
    (when (> x 5)
      (stop "end"))))

(defuncc run-forever ()
  (generator-loop (x) (even-numbers)
    (print x))

  #+nil
  (cc-context (run-forever)))
