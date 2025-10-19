;;; package.lisp
;;; -----------
;;; This file defines the package and namespace for the Bryce Evolution System.
;;; It imports core symbols from CL and py4cl, and selected tools from lparallel
;;; for multi-threading in parallel fitness evaluation

(defpackage :bes
  (:use :cl :py4cl)
  (:import-from :lparallel
                #:*kernel*
                #:make-kernel
                #:pmap
                #:end-kernel)
  (:shadow :report)
  (:export evolve defdataset defexperiment execute-team))

(in-package :bes)
                
               
