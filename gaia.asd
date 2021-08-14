;;;; gaia.asd

(asdf:defsystem #:gaia
  :description "Gaia allows easy usage of genetic and evolutionary algorithms."
  :author "Timo Netzer <exodiquas@gmail.com>"
  :license  "WTFPL"
  :version "0.0.1"
  :serial t
  :components ((:file "src/package")
	       (:file "src/gaia")
	       (:file "src/individual")
	       (:file "src/population")))

(asdf:defsystem #:gaia-test
  :description "Tests for the Gaia system."
  :author "Timo Netzer <exodiquas@gmail.com>"
  :license  "WTFPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:gaia
	       #:fiveam)
  :components ((:file "t/individual")
	       (:file "t/population")))
