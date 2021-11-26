(in-package #:cl-gen)

(defgen boris2 ()
  (print "a generata")
  (yield-bind (x) (progn (format t "Give me a numbah!~%") 34)
    (stop 55)
    (print $stop)
    (yield-bind (y) (values 420 (format t "evaled x ~A~%" x))
      (yield-bind (z) (format t "evaled y ~A~%" y)
        (format t "evaled z ~A~%" z)))))

(cc-context (boris2))

(cc-context (defparameter g (boris2)))

(defuncc izi ()
  (next-bind (x) (g 1 2 3 4)
    (format t "current: ~A~%" x)
    (next-bind (x y) (g)
      (format t "currents: ~A ~A~%" x y)
      (next-bind () (g)))
    (next-bind (x) (g "420 mtkas") 55)))

(cc-context (izi))

(defuncc boris-consume ()
  (generator-context (boris2)
    (()
     (format t "current: ~A~%" current)
     (next (3)
       (format t "current: ~A~%" current)
       (next (5)
         (format t "current: ~A~%" current)
         (next (6)
           (format t "current: ~A~%" current)
           (next (5555)
             (format t "Shite shouldve happend")))) 
       (next (1))
       (next (5)
         (format t "current: ~A~%" current)))
     (format t "current: ~A~%" current))))

(cc-context (boris-consume))

(defgen generatorzin ()
  (yield-bind (x) (format t "2 + 2 = ") 
    (if (= x 4)
        (format t "~A~%" x)
        (format t "So dumb lmao~%"))
    (yield-bind (x) (format t "3 * 3 = ") 
      (if (= x 4)
          (format t "~A~%" x)
          (format t "So dumb lmao~%"))
      (format t "~A~%" x))))

(cc-context
  (generator-context (generatorzin)
    (next ()
      (next ((read))
        (next ((read)))))))

(defgen generate-numbers ()
  (labels ((rec (x)
             (yield-bind (&optional (inc 1)) x
               (rec (+ x inc)))))
    (rec 0)))

(cc-context (generate-numbers))

(defmacro inf-seq (binding &body body)
  (labels ((rec (x)
             (yield-bind (&optional (inc 1)) x
               (rec (+ x inc)))))
    (rec 0)))

(cc-context
  (cc-bind () (values $cc 3)
    (print "waddap")))
