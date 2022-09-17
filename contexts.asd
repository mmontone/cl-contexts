(asdf:defsystem :contexts
  :version      "0.1.0"
  :description  "Dependency injection library for Common Lisp"
  :author       "Mariano Montone <marianomontone@gmail.com>"
  :serial       t
  :license      "MIT"
  :components   ((:file "contexts")
		 (:file "contexts-modules")
		 (:file "modules-example"))
  :depends-on   (#:alexandria #:lisp-namespace))
