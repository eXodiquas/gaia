;;;; gaia.asd

(asdf:defsystem #:gaia
  :description "Gaia allows easy usage of genetic and evolutionary algorithms."
  :author "Timo Netzer <exodiquas@gmail.com>"
  :license  "WTFPL"
  :version "0.0.1"
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "gaia")
                 (:file "individual")))))
