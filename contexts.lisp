(defpackage :contexts
  (:use :cl :lisp-namespace)
  (:export
   #:let-context
   #:let-context*
   #:with-contexts
   #:with-contexts*
   #:defcontext
   #:find-context
   #:context-value
   #:bind-contexts))

(in-package :contexts)

(defvar *context* nil "The current context")

(define-namespace context)

(defun find-context (name)
  (symbol-context name))

(defmacro defcontext (name &body bindings)
  "Define a context.
Bindings is an association list with (binding-name . value)."
  `(setf (symbol-context ',name)
	 ',bindings))

(defun bind-contexts (contexts &optional (context *context*))
  "Bind CONTEXTS to current CONTEXT."
  (append
   (apply #'append (mapcar #'find-context contexts))
   (when context
     (list context))))

(defmacro with-contexts (contexts &body body)
  "Evaluate BODY in context of CONTEXTS."
  `(let ((*context* (bind-contexts ',contexts)))
     ,@body))

(defmacro with-contexts* (context contexts &body body)
  "Evaluate BODY in context of CONTEXTS."
  `(let ((,context (bind-contexts ',contexts ,context)))
     ,@body))

(defun context-value (name &optional (context *context*))
  "Get the value of NAME from current CONTEXT.
If not bound, an error is signaled."
  (loop for (binding-name . val) in context
	when (string= (symbol-name binding-name)
		      (symbol-name name))
	  do (return-from context-value val))
  (error "Not bound in current context: ~s" name))

(defmacro let-context (bindings &body body)
  "Bind variables from current context."
  `(let ,(loop for binding in bindings
		collect `(,binding (context-value ',binding)))
     ,@body))

(defmacro let-context* (context bindings &body body)
  "Bind variables from a specific CONTEXT."
  `(let ,(loop for binding in bindings
		collect `(,binding (context-value ',binding ,context)))
     ,@body))
