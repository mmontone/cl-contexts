(defpackage :contexts-tests
  (:use :cl :stefil))

(in-package :contexts-tests)

(deftest contexts-module-test ()

  (contexts.modules:reset-module 'my-database-module)
  
  (signals error
    (contexts.modules:context-of 'my-database-module))
  
  ;; This call signals context error: Not bound in current context: %DB-DRIVER
  (signals error
    (my-database-module:connect-db))

  ;; Initialize context variables in module
  (contexts.modules:init-module
   'my-database-module
   '((%db-driver . :postgres)
     (%db-host . "localhost")
     (%db-port . 5432)))

  ;; Now the call to connect-db works:
  (finishes (my-database-module:connect-db))

  ;; Module not defined:
  (signals error
    (contexts.modules:init-module 'lala '((db-name . 22))))

  ;; Context var required:
  (signals error
    (contexts.modules:init-module 'my-database-module
				  '((%db-driver . :postgres))))

  ;; Use default value for port:
  (contexts.modules:init-module 'my-database-module
				'((%db-driver . :postgres)
				  (%db-host . "localhost")))
  (is (= my-database-module::%db-port 5432))
  (finishes (my-database-module:connect-db))
  
  ;; Fine:
  (finishes
    (contexts.modules:init-module 'my-database-module
				  '((%db-driver . :postgres)
				    (%db-host . "localhost")
				    (%db-port . 2020))))
  (finishes (my-database-module:connect-db))

  ;; Var is not part of context:
  (signals error
    (contexts.modules:init-module 'my-database-module
				  '((%foo . "foo")
				    (%db-driver . :postgres)
				    (%db-host . "localhost")
				    (%db-port . 2020))))

  ;; Type checking error
  (signals error
    (contexts.modules:init-module 'my-database-module
				  '((%db-driver . :postgres)
				    (%db-host . "localhost")
				    (%db-port . "2020")))))
