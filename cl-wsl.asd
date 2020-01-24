(defsystem "cl-wsl"
  :version "0.1.0"
  :author "Dinesh Weerapurage <xydinesh@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("rove"
               "split-sequence"
               "hunchentoot")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "World Softball League"
  :in-order-to ((test-op (test-op "cl-wsl/tests"))))

(defsystem "cl-wsl/tests"
  :author "Dinesh Weerapurage <xydinesh@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("cl-wsl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
		 (:file "rounds")
		 (:file "team"))))
  :description "World Softball League Tests"
  :perform (test-op (op c) (symbol-call :rove :run c)))
