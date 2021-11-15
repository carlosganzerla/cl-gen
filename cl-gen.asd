;;;; cl-gen.asd

(asdf:defsystem #:cl-gen
  :description "JS-like generators in Common Lisp"
  :author "Carlo Sganzerla <your.name@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "cl-gen")))
