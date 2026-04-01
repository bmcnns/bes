(asdf:defsystem "bes"
  :description "Bryce Evolution System"
  :version "0.3"
  :author "Bryce MacInnis"
  :license "GPL-3"
  :depends-on ("lparallel" "py4cl" "ironclad")
  :components ((:file "package")
               (:file "utils")
               (:file "instructions")
               (:file "dataset")
               (:file "experiments")
               (:file "fitness")
               (:file "program")
               (:file "vm")
               (:file "mutation")
               (:file "selection")
               (:file "reporter")
               (:file "learner")
               (:file "team")
               (:file "graph")
               (:file "tpg")
               (:file "breeder")
               (:file "selection-dsl")
               (:file "evolution-dsl")
               (:file "unit-test")
               (:file "tests/test-tpg")
               (:file "tests/test-mutation")
               (:file "tests/test-fitness")
               (:file "tests/test-selection")))
