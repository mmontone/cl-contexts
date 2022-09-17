(defpackage :lambda-syntax
  (:use :cl)
  (:export :lambda-reader))

(in-package :lambda-syntax)

(defun replace-atoms (form replacer)
  (if (atom form)
      (funcall replacer form)
      (cons (replace-atoms (car form) replacer)
	    (replace-atoms (cdr form) replacer))))

(defun parse-implicit-arg (symbol)
  (let ((symbol-name (symbol-name symbol)))
    (cond
      ((string= symbol-name "%&")
       '&rest)
      ((string= symbol-name "%")
       0)
      ((and (char= (aref symbol-name 0) #\%)
	    (every #'digit-char-p (subseq symbol-name 1)))
       (1- (parse-integer (subseq symbol-name 1)))))))

;; (parse-implicit-arg '%)
;; (parse-implicit-arg '%1)
;; (parse-implicit-arg '%2)
;; (parse-implicit-arg '%%1)
;; (parse-implicit-arg '%&)

(defun make-implicit-lambda (form)
  "Make a lambda expression from FORM.
- % and %1 for referencing the first argument.
- %2, %3 .. %n for referencing the nth argument.
- %& for referencing the rest of the arguments after the highest individually referenced argument."
  (let* (implicit-args
	 (args (gensym "args"))
	 (body (replace-atoms form
			      (lambda (atom)
				(alexandria:if-let
				    ((arg (and (symbolp atom)
					       (parse-implicit-arg atom))))
				  (progn
				    (pushnew arg implicit-args)
				    (cond
				      ((equalp arg '&rest)
				       `rest-args)
				      ((numberp arg)
				       `(nth ,arg ,args))))
				  atom)))))
    `(lambda (&rest ,args)
       ,body)))

;; (make-implicit-lambda '(+ %1 %2))
;; (make-implicit-lambda '(print %))

;; (funcall (eval (make-lambda '(+ %1 %2))) 34 54)			      

(defun lambda-reader (stream char arg &key (recursive-p t))
  "The actual reader function for the 'sub-character' #\f.

This function can be used directly outside of a read table by passing `recursive-p` as NIL."
  (declare (ignore char arg recursive-p))
  (let ((form (read stream)))
    (make-implicit-lambda form)))

;;(with-input-from-string (s "(+ %1 %2)")
;;  (lambda-reader s nil nil))

;; (get-dispatch-macro-character #\# #\l)

(cl-syntax:defsyntax implicit-lambda
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\l #'lambda-reader))

(cl-syntax:use-syntax implicit-lambda)

;; (mapcar #l(subseq (package-name %) 1) (list-all-packages))
