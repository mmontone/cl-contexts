(defpackage :package-star
  (:use :cl)
  (:nicknames :package*)
  (:local-nicknames
   (:alex :alexandria))
  (:export
   #:package*
   #:defpackage*
   #:in-package*
   #:find-package*)
  (:documentation "Extensible package system."))

(in-package :package-star)

(defvar *packages* (make-hash-table :test 'equalp)
  "The collection of defined packages.")

(defvar +cl-defpackage-options+
  '(:use :export :import-from :shadow
    :shadowing-import-from :local-nicknames
    :lock :implement :documentation :intern :size
    :nicknames))

(defstruct package*
  name
  options)

(defun package*:find-package* (package-name)
  (gethash package-name package*::*packages*))

(defun collect-cl-defpackage-options (options)
  "Collection OPTIONS used by standard CL:DEFPACKAGE."
  (remove-if-not (alex:compose (alex:rcurry #'member +cl-defpackage-options+)
			       #'car)
		 options))

;; (collect-cl-defpackage-options '((:use "lala")(:foo "adsf")))

(defun collect-non-cl-defpackage-options (options)
  "Collection OPTIONS not used by standard CL:DEFPACKAGE."
  (remove-if (alex:compose (alex:rcurry #'member +cl-defpackage-options+)
			       #'car)
	     options))

;; (collect-non-cl-defpackage-options '((:use "lala")(:foo "adsf")))

(defmacro defpackage* (name &rest options)
  "Defines a new extensible package called with NAME.
Each of OPTIONS has the form (option-name &rest args)."
  `(let ((package* (make-package* :name ',name :options ',options)))
     (defpackage ,name
       ,@(collect-cl-defpackage-options options))
     ,@(loop for option in (collect-non-cl-defpackage-options options)
	     collect `(init-package*-option package*
					    ',(car option)
					    ',option))
     (setf (gethash ,(symbol-name name) *packages*) package*)
     package*))

(defgeneric init-package*-option (package* option-name option)
  (:documentation "Initialize PACKAGE* option named with OPTION-NAME.
This is evaluated at package definition time."))

(defmethod init-package*-option (package* option-name option)
  (error "Uknown package option: ~a" option-name))

(defgeneric process-in-package*-option (package* option-name option)
  (:documentation "Process PACKAGE* OPTION at IN-PACKAGE* time."))

(defmethod process-in-package*-option (package* option-name option)
  "Do nothing by default"
  nil)

(defun process-in-package* (package)
  "Process options for IN-PACKAGE calls."
  (dolist (option (collect-non-cl-defpackage-options (package*-options package)))
    (process-in-package*-option package (first option) option)))

(defmacro package*:in-package* (package-name)
  `(progn
     (cl:in-package ,package-name)
     (package*::process-in-package* (package*:find-package* ',package-name))))
