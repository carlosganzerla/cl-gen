;;;; cl-gen.asd

(asdf:defsystem #:cl-gen
  :description "JS-like generators in Common Lisp"
  :author "Carlo Sganzerla <maaprd.carlo@gmail.com>"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "utils")
               (:file "continuations")
               (:file "generators")))
