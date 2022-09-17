;; Define cl-interpol syntax
(cl-syntax:defsyntax interpol-syntax
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\? #'cl-interpol::interpol-reader))

(package*:defpackage* :interpol-syntax-example
    (:use :cl)
  (:syntax interpol-syntax))

(package*:in-package* :interpol-syntax-example)

;;(IN-PACKAGE :INTERPOL-SYNTAX-EXAMPLE)
;;(CL-SYNTAX:USE-SYNTAX 'INTERPOL-SYNTAX)

(let ((what "world"))
  #?"hello ${what}")
