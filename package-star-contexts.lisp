(defpackage :package-star/contexts
  (:use :cl))

(in-package :package-star/contexts)

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

(defmethod package-star:init-package*-option (package* (option-name (eql :context)) option)
  (let ((context (package*:find-package*-option package* :context))
	(package-name (package-star::package*-name package*)))
    (loop for binding in (cdr context)
          for binding-name := (cond
				((symbolp binding)
				 (intern (symbol-name binding) package-name))
				((listp binding)
				 (intern (symbol-name (first binding)) package-name))
				(t "Invalid syntax for context binding: ~s" binding))
	  do  (eval
	       `(define-symbol-macro ,binding-name
                    (contexts:context-value ',binding-name (get-package-context ',package-name)))))))
