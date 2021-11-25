(in-package #:cl-gen)

(defgen boris2 ()
  (yield-bind (progn (format t "Give me a numbah!~%") 34) (x)
    (yield-bind (values 420 (format t "evaled x ~A~%" x)) (y) 
      (yield-bind (format t "evaled y ~A~%" y) (z) 
        (format t "evaled z ~A~%" z)))))

(cc-context (boris2))

(defuncc izi ()
  (next-bind (g x) ((boris2))
    (format t "current: ~A~%" x)
    (next-bind (g x y) (g)
      (format t "currents: ~A ~A~%" x y)
      (next-bind (g) (g)))
    33))

(cc-context (izi))

(defuncc boris-consume ()
  (generator-context (boris2)
    (next ()
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
  (yield-bind (format t "2 + 2 = ") (x)
    (if (= x 4)
     (format t "~A~%" x)
     (format t "So dumb lmao~%"))
    (yield-bind (format t "3 * 3 = ") (x)
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
             (yield-bind x (&optional (inc 1))
               (rec (+ x inc)))))
    (rec 0)))

(cc-context (generate-numbers))

(defmacro inf-seq (binding &body body)
  (labels ((rec (x)
             (yield-bind x (&optional (inc 1))
               (rec (+ x inc)))))
    (rec 0)))

(cc-context
  (cc-bind () (values $cc 3)
    (print "waddap")))
