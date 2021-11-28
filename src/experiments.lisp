(in-package #:cl-gen)

(defgen boris2 ()
  (print "a generata")
  (yield-bind (x) (progn (format t "Give me a numbah!~%") 34)
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

(defgen generatorzin ()
  (format t "2 + 2 = ")
  (yield-bind (x) ()
    (if (= x 4)
        (format t "~A~%" x)
        (format t "So dumb lmao~%"))
    (format t "3 * 3 = ")
    (yield-bind (x) ()  
      (if (= x 9)
          (format t "~A~%" x)
          (format t "So dumb lmao~%")))))

(cc-context (generatorzin))

(cc-context
  (let ((g (generatorzin)))
    (next-bind () (g)
      (next-bind () (g (read))))))

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

(defgen numbers ()
  (yield-bind () 1
    (yield-bind () 2
      (yield-bind () 3))))

(cc-context 
  (generator-bind (x) (numbers) 
    (print x)))

(cc-context 
  (generator-bind () (boris2)))



(defgen test ()
  (generator-do ((x 0 (1+ x)))
                (nil)
                x))

(cc-context (test))

(cc-context 
  (generator-bind (x) (test)
    (if (> x 5)
        (stop nil)) 
    (print x)))

(defgen read-str (str)
  (let ((lst nil))
    (generator-do ((c (read-char str nil :eof) (read-char str nil :eof)))
                  ((eql c :eof) (nreverse lst))
                  (push c lst))))

(defuncc some-loop (val)
  (labels ((rec ()
             (yield-bind (x) val
               (if (eql x :end)
                 (stop "Game over biatchhh" x)
                 (rec))))) 
    (rec)))

(cc-context 
  (some-loop 3)) 
