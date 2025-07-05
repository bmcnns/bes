(asdf:defsystem "bes"
  :description "Bryce Evolution System"
  :version "0.1"
  :author "Bryce MacInnis"
  :license "GPL-3"
  :depends-on ("lparallel" "py4cl")
  :components ((:file "package")
               (:file "utils")
               (:file "genotype")
               (:file "macros")
               (:file "instructions")
               (:file "dataset")
               (:file "hopper")
               (:file "experiments")
               (:file "phenotype")
               (:file "mutation")
               (:file "fitness")
               (:file "selection")
               (:file "plotting")))
