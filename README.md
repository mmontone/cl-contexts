# CL-CONTEXTS

This is just an experiment.

The idea is to extend Common Lisp package system with "context" variables.
Context variables are variables required by a package (that I call modules).
For the functions of a module to function, a module needs to initialized first with proper context variable values.
The idea is to have some sort of dependency injection at the package/module level.

An example:

Define a module:

```lisp
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
```

Then initialize and use:
```lisp
;; This call signals an error because the module context has not been initialized yet:
(my-database-module:connect-db)

;; Initialize context variables in module
(contexts.modules:init-module
  'my-database-module
  '((%db-driver . :postgres)
    (%db-host . "localhost")
    (%db-port . 5432)))

;; Now the call to connect-db works:
(my-database-module:connect-db)
```
