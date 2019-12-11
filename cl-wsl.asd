;;;; cl-wsl.asd

(asdf:defsystem #:cl-wsl
  :description "World Wide Softball League"
  :author "Dinesh Weerapurage <xydinesh@gmail.com>"
  :license  "Apache 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot)
  :components ((:file "package")
               (:file "cl-wsl")))
