(cl:defpackage #:reserpt
  (:use :cl)
  (:nicknames #:r)
  (:export #:*do-tests-when-defined*
           #:*compile-tests*
           #:*test*
           #:continue-testing
           #:deftest
           #:do-test
           #:do-tests
           #:get-test
           #:pending-tests
           #:rem-all-tests
           #:rem-test
           #:defnote
           #:my-aref
           #:*catch-errors*
           #:disable-note))
