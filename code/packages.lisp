(cl:defpackage #:reserpt
  (:use :cl)
  (:nicknames #:r)
  (:export #:*compile-tests*
           #:*test*
           #:continue-testing
           #:deftest
           #:do-test
           #:do-tests
           #:get-test
           #:defnote
           #:*catch-errors*))
