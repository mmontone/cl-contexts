(defpackage :contexts.modules
  (:use :cl)
  (:export
   #:defmodule
   #:init-module
   #:reset-module
   #:context-of))

(in-package :contexts.modules)

(defvar *contexts-modules* (make-hash-table :test 'equalp)
  "The collection of defined modules.")

(defun set-package-context (package context)
  "Set the context for PACKAGE."
  (setf (symbol-value (intern "*MODULE-CONTEXT*" package))
        context))

(defun get-package-context (package)
  "Get the current context for PACKAGE."
  (let ((binding (intern "*MODULE-CONTEXT*" package)))
    (when (not (boundp binding))
      (error "Module ~a not initialized. Call CONTEXTS.MODULES:INIT-MODULE." package))
    (symbol-value binding)))

(defun context-of (module-name)
  "Get the context of module."
  (get-package-context module-name))

(defun reset-module (module-name)
  "Reset the module context."
  (let ((module-context (cdr (find :context (gethash (string module-name) *contexts-modules*) :key #'car))))
    (when (not module-context)
      (error "Module not defined: ~a" module-name))
    ;; Unbind the module context
    (makunbound (intern "*MODULE-CONTEXT*" module-name))))

(defmacro defmodule (name &body options)
  "Define a module.
Unlike packages, a module can include :context definitions.
A normal CL:PACKAGE is defined in the process."
  (let ((context (find :context options :key #'car))
        (package-options (remove :context options :key #'car)))
    ;; Register module in the global table of modules
    (setf (gethash (string name) *contexts-modules*) options)
    ;; Create a package. We use eval. Is there a better way?
    ;; We cannot simply macro expand to CL:DEFPACKAGE because
    ;; we need to intern symbols right after in this macro ..
    (eval `(cl:defpackage ,name ,@package-options))
    ;; Process the context variables
    ;; Context variables are defined as symbol macros that access the current
    ;; module context.
    (let ((*package* (find-package name)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (cl:defpackage ,name ,@package-options)
         ,@(loop for binding in (cdr context)
                 for binding-name := (cond
				       ((symbolp binding)
					(intern (symbol-name binding) name))
				       ((listp binding)
					(intern (symbol-name (first binding)) name))
				       (t "Invalid syntax for context binding: ~s" binding))
		 collect `(define-symbol-macro ,binding-name
                              (contexts:context-value ',binding-name (get-package-context ',name))))))))

;; Problem with IN-MODULE is that it does not play well with SLIME.
;; We need to use CL:IN-PACKAGE instead if we want slime to detect current package correctly when working with files.
(defmacro in-module (string-designator)
  `(cl:in-package ,string-designator))

(defun init-module (module-name args)
  "Initialize the module named with MODULE-NAME.
ARGS is an alist of (context-var . value)."
  (let ((module-context (cdr (find :context (gethash (string module-name) *contexts-modules*) :key #'car))))
    (when (not module-context)
      (error "Module not defined: ~a" module-name))
    (let ((args-values
	    (loop for context-var in module-context
		  for context-var-name := (cond
					    ((symbolp context-var) context-var)
					    ((listp context-var)
					     (first context-var)))
		  for context-var-type := (and (listp context-var)
					       (getf (cdr context-var) :type))
		  for context-var-initform := (and (listp context-var)
						   (getf (cdr context-var) :initform))
		  for arg := (assoc (symbol-name context-var-name)
				    args
				    :key #'symbol-name
				    :test #'string=)
		  do
		     ;; Signal error when a context variable value has not been assigned
		     (when (and (not arg)
				(not context-var-initform))
		       (error "Context var required: ~a" context-var-name))
		     ;; Signal error when the context variable type and value do not match
		     (when (and arg context-var-type (not (typep (cdr arg) context-var-type)))
			 (error "Type error. ~a: ~a is not of type ~a" context-var-name arg context-var-type))
		  collect (if arg
			      arg
			      (cons context-var-name (eval context-var-initform))))))
      (let ((context-var-names (loop for context-var in module-context
				     collect (cond
					       ((symbolp context-var)
						(symbol-name context-var))
					       ((listp context-var)
						(symbol-name (first context-var)))))))
	(loop for arg-name in (mapcar #'car args)
	      when (not (member (symbol-name arg-name)
				context-var-names
				:test #'string=))
		do (error "~s is not part of this context." arg-name)))
      (set-package-context module-name args-values))))
