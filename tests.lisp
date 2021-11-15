(defun js-test ()
  (yield 1)
  (yield 2)
  (yield 3)
  (print "fodase")
  (yield 4)
  (yield 5))

(generator-consume (x (js-test))
  (stop-when () x))

(defun loop1 ()
  (do ((x 0 (1+ x))) 
      (nil)
      (yield x)))

(defun loop2 ()
  (generator-consume (x (loop1))
    (when (evenp x)
      (yield x))))

(defun loop3 ()
  (generator-consume (x (loop2))
    (print x)
    (stop-when (> x 100) "deu porra")))

