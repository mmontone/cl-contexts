(package*:defpackage* :my-package
    (:use :cl))

(package*:in-package* :my-package)

(package*:find-package* :my-package)

(package*:defpackage* :readtable-package
    (:use :cl)
  (:readtable :modern))

(package*:find-package* :readtable-package)

(package*:in-package* :readtable-package)


