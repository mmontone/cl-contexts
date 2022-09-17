(defpackage :package-star/readtables
  (:nicknames :package*/readtables)
  (:use :cl))

(in-package :package-star/readtables)

(defmethod package*:init-package*-option (package* (option-name (eql :readtable)) option)
  (let ((readtable-name (cadr option)))
    (or (named-readtables:find-readtable readtable-name)
	(error "Readtable not found: ~s" readtable-name))))

(defmethod package*:process-in-package*-option (package* (option-name (eql :readtable)) option)
  (let ((readtable-name (cadr (package*:find-package*-option package* :readtable))))
    `(named-readtables:in-readtable ,readtable-name)))
