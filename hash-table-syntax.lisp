(defpackage :hash-table-syntax
  (:use :cl)
  (:export :hash-table-reader))

(in-package :hash-table-syntax)

(defun hash-table-reader (stream char arg &key (recursive-p t))
  "Read hash-table literal expression."
  (declare (ignore char arg))

  (let ((form (read-delimited-list #\} stream recursive-p))
	(hash-table (gensym "hash-table-")))
    `(let ((,hash-table (make-hash-table)))
       ,@(loop for key in form by #'cddr
	       for value in (cdr form) by #'cddr
	       collect `(setf (gethash ,key ,hash-table) ,value))
       ,hash-table)))

;; (with-input-from-string (s ":a :b }") (hash-table-reader s nil nil :recursive-p nil))

;; (with-input-from-string (s ":a :b { :c 'aaa } }") (hash-table-reader s nil nil :recursive-p nil))

(cl-syntax:defsyntax literal-hash-table
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\{ #'hash-table-reader))

(cl-syntax:use-syntax literal-hash-table)

;; (let ((table #{ :a 22 :foo "lala" }))
;;   table)

;; (let ((table #{ :a 22 :foo #{ :x 44 } }))
;;   table)
