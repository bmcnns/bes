(asdf:defsystem "bes"
  :description "Bryce Evolution System"
  :version "0.2"
  :author "Bryce MacInnis"
  :license "GPL-3"
  :depends-on ("lparallel" "py4cl")
  :components ((:file "package")
               (:file "utils")
               (:file "instructions")
               (:file "dataset")
               (:file "experiments")
               (:file "experiments/hopper")
               (:file "fitness")
               (:file "program")
               (:file "mutation")
               (:file "selection")
               (:file "evolution")
               (:file "reporter")
               (:file "learner")
               (:file "team")
               (:file "graph")
               (:file "tpg")
               (:file "pareto")
               (:file "linear-gp")
               (:file "unit-test")
               (:file "tests/test-tpg")
               (:file "tests/test-mutation")
               (:file "tests/test-fitness")
               (:file "tests/test-selection")))
