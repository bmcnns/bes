(in-package :bes)

(defparameter *program-id-generator* (make-unique-id-generator "P"))

(defun program-p (form)
  (and
   (listp form)
   (equal (car form) 'PROGRAM)
   (symbolp (cadr form))
   (listp (caddr form))
   (listp (caaddr form))))

(defun program-id (program)
  (unless (program-p program)
    (error "PROGRAM-ID expects a PROGRAM. Got ~A instead.~%" program))
  (cadr program))

(defun program-instructions (program)
  (unless (program-p program)
    (error "PROGRAM-INSTRUCTIONS expects a PROGRAM. Got ~A instead.~%" program))
  (caddr program))

(defun execute-program (program observations)
  (let ((instructions (caddr program)))
    (if (program-p program)
        (phenotype instructions observations)
        (error "Tried to execute a program but the thing you're trying to execute~%is not a program. ~A" program))))

(defun eval-program (program dataset)
  (let* ((observations (observations dataset))
         (actions (actions dataset))
         (predictions (mapcar (lambda (obs) (execute-program program obs)) observations)))
    (fitness program actions predictions)))
