(defpackage :package-star/syntax
  (:nicknames :package*/syntax)
  (:use :cl))

(in-package :package-star/syntax)

(defmethod package*:init-package*-option (package* (option-name (eql :syntax)) option)
  (let ((syntax-name (cadr option)))
    (or (cl-syntax:find-syntax syntax-name)
	(error "Syntax not found: ~s" syntax-name))))

(defmethod package*:process-in-package*-option (package* (option-name (eql :syntax)) option)
  (let ((syntax-name (cadr (package*:find-package*-option package* :syntax))))
    `(cl-syntax:use-syntax ',syntax-name)))
