;;;; package.lisp

(defpackage #:cl-gen
  (:use #:cl)
  (:export #:yield #:stop #:stop-when #:stop-unless #:generator-bind #:next
           #:generator-collect))
