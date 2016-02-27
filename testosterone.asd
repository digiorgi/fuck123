;;;; testoterone.asd

(asdf:defsystem #:testosterone
  :description "My custom test framework"
  :author "Di Giorgi Hernan Ezequiel <h@srt.onl>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "testosterone")))
