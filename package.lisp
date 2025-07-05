(defpackage :bes
  (:use :cl :py4cl)
  (:import-from :lparallel
                #:*kernel*
                #:make-kernel
                #:pmap
                #:end-kernel))

(in-package :bes)
                
               
