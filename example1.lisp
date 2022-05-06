(defpackage :contexts.example1
  (:use :cl :contexts))

(in-package :contexts.example1)

;; Example

(defun db-driver-connect (driver host port)
  (format t "Connecting db: ~a~%" (list driver host port)))

;; connect-db binds variables from context
(defun connect-db ()
  ;; use let-context to bind variable values from context
  (let-context (db-driver db-host db-port)
    (db-driver-connect db-driver db-host db-port)))

;; This call signals error: "Not bound in current context: DB-DRIVER"
(connect-db)

;; Define a context
(defcontext prod-db
  (db-host . "localhost")
  (db-port . 5432)
  (db-driver . :postgres))

;; and use it for the call
(with-contexts (prod-db)
  (connect-db))

(with-contexts* contexts::*context* (prod-db)
  (connect-db))
