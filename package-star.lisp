;; Experimental extensible package system
;; Packages get a generic syntax (defpackage* clauses), where
;; each clause can be implemented by independent systems.
;; Possible use cases:
;; - Useful 
;; TODO:
;; - See if it is possible to adapt UIOP/PACKAGE:DEFPACKAGE (:REEXPORT AND :MIX options)
;; - Conduits
;; - Contexts
;; - Readtables and syntax tables.

(defpackage :package-star
  (:use :cl)
  (:nicknames :package*)
  (:local-nicknames
   (:alex :alexandria))
  (:export
   #:package*
   #:defpackage*
   #:in-package*
   #:find-package*
   #:init-package*-option
   #:process-in-package*-option
   #:find-package*-option)
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
  "Find package named with PACKAGE-NAME."
  (gethash (string package-name) package*::*packages*))

(defun package*:find-package*-option (package option-name)
  "Find PACKAGE option named with OPTION-NAME."
  (find option-name (package*-options package) :key #'car))

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
     ,@(let ((package* (package*:find-package* package-name)))
	 (loop for option in (package*::collect-non-cl-defpackage-options
			      (package*::package*-options package*))
	     collect (package*:process-in-package*-option package* (car option) option)))))
       
