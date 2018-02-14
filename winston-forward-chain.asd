;;;; -*- Mode: LISP -*-

(in-package :asdf)


(asdf:defsystem winston-forward-chain
  :author "Patrick Winston"
  :default-component-class cl-source-file.lsp
  :components
   ((:file "package")
    (:file "25stream")
    (:file "24matchi")
    (:file "26forwar"))
  :serial t)
