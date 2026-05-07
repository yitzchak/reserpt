(asdf:defsystem "reserpt"
  :description "A regression testing system derived from RT"
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/yitzchak/reserpt"
  :bug-tracker "https://github.com/yitzchak/reserpt/issues"
  :in-order-to ((asdf:test-op (asdf:test-op #:reserpt/test)))
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "impl")))))

(asdf:defsystem "reserpt/test"
  :description "Test suite for reserpt"
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/yitzchak/reserpt"
  :bug-tracker "https://github.com/yitzchak/reserpt/issues"
  :depends-on ("reserpt")
  :perform (asdf:test-op (op c) (uiop:symbol-call :reserpt :do-tests :exit t))
  :components ((:module code
                :pathname "code/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))
