(defpackage :bes
  (:use :cl :py4cl)
  (:import-from :lparallel
                #:*kernel*
                #:make-kernel
                #:pmap
                #:end-kernel)
  (:export evolve defdataset defexperiment))

(in-package :bes)
                
               
