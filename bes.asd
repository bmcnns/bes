(asdf:defsystem "bes"
  :description "Better Evolution System"
  :version "0.4"
  :author "Bryce MacInnis"
  :license "GPL-3"
  :depends-on ("usocket" "bordeaux-threads" "lparallel")
  :components ((:file "package")
	       (:file "globals")
	       (:file "helpers")
	       (:file "instruction")
	       (:file "program")
	       (:file "migration")
               (:file "networking")
	       (:file "main")))
