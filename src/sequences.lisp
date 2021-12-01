(defpackage #:sequences
  (:use #:cl #:cl-gen))
(in-package #:sequences)

(defgen generate-seq (&key (init 0) (end nil) (step 1))
  (do-yield ((x init (+ x step)))
            ((and end (>= x end)))
            x))

(defgen filter-seq (seq pred)
  (generator-bind (x) seq
    (if (funcall pred x)
        (yield x)
        (next))))

(defgen map-seq (seq mapper)
  (generator-bind (x) seq
    (yield (funcall mapper x))))

(defgen duplicate-seq (seq)
  (generator-bind (x) seq
    (yield-bind () x
      (yield x))))

(defgen take (seq count)
  (let ((left count))
    (generator-bind (x) seq
      (when (> left 0)
        (decf left)
        (yield x)))))

(defgen skip (seq count)
  (let ((left count))
    (generator-bind (x) seq
      (if (= left 0)
          (yield x)
          (progn (decf left)
                 (next))))))

(defuncc print-seq (seq)
  (generator-loop (x) seq
    (print x)))

(cc-context
  (generator-loop (x) (generate-seq :end 5)
    (print x)))

(defgen even-seq (seq)
  (generator-bind (x) seq
    (if (evenp x)
      (yield x)
      (next))))

(cc-context
  (generator-loop (x) (generate-seq :end 5)
    (print x)))

(cc-context
  (generator-loop (x) (even-seq (generate-seq :end 10))
    (print x)))

(cc-context
  (print-seq (duplicate-seq
               (map-seq (take (filter-seq (skip (generate-seq) 500)
                                          #'evenp)
                              20)
                        #'list))))
