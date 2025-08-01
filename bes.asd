(asdf:defsystem "bes"
  :description "Bryce Evolution System"
  :version "0.1"
  :author "Bryce MacInnis"
  :license "GPL-3"
  :depends-on ("lparallel" "py4cl")
  :components ((:file "package")
               (:file "utils")
               (:file "genotype")
               (:file "instructions")
               (:file "dataset")
               (:file "experiments")
               (:file "phenotype")
               (:file "mutation")
               (:file "selection")
               (:file "fitness")
               (:file "evolution")
               (:file "reporter")))

