(in-package :bes)

(defparameter *memory-size* 8)

(defvar *memory* (make-array *memory-size* :initial-element 0.0 :element-type 'single-float))

(declaim (type (simple-array single-float (*)) *memory*))

(defun reset-memory ()
  (fill *memory* 0.0f0))

