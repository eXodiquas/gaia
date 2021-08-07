;;;; gaia-test.asd

(asdf:defsystem #:gaia-test
  :author "Timo Netzer <exodiquas@gmail.com>"
  :license  "WTFPL"
  :depends-on ("gaia"
               "fiveam")
  :components ((:module "t"
                :serial t
                :components
                ((:file "individual")))))
