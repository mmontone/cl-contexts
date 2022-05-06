;; Define a module with context variables
(contexts.modules:defmodule my-database-module
  (:use :cl)
  (:export #:connect-db)
  ;; The context variables ..
  ;; I prefix context variables with % so that they can be distinguished from
  ;; other variables, but it is not obligatory .
  (:context
   (%db-driver :type keyword)
   %db-host
   (%db-port :type integer
	     :initform 5432
	     :documentation "The database port")))

(cl:in-package my-database-module)

(defun driver-connect-db (driver host port)
  (format t "Connecting to db: ~s~%" (list driver host port)))

;; Database connection function that uses variables from module context
(defun connect-db ()
  (driver-connect-db %db-driver %db-host %db-port))
