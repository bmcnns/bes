(in-package :bes)

(defun programs (lgp)
  "Returns the PROGRAMs of a LINEAR GP"
  (unless (linear-gp-p lgp)
    (error "Expecting a Linear GP. Got ~A instead.~%" lgp))
  (cdr lgp))

(defun build-program-table (lgp)
  (let ((program-table (make-hash-table :test 'equal)))
    (dolist (program (programs lgp))
      (setf (gethash (program-id program) program-table) program))
    program-table))
  
(defun make-linear-gp ()
  (let* ((population-size (experiment-population-size *experiment*))
         (programs (loop repeat population-size
                         collect (make-program))))
    `(LINEAR-GP ,@programs)))

(defun linear-gp-p (form)
  (and (equal (car form) 'LINEAR-GP)
       (every #'program-p (cdr form))))

(defun eval-linear-gp (lgp dataset)
  (let ((programs (programs lgp))
        (num-threads (experiment-num-threads *experiment*)))
    (with-population programs num-threads
      (eval-program individual dataset))))
