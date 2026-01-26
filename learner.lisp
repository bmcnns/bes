(in-package :bes)

(defparameter *learner-id-generator* (make-unique-id-generator "L"))

(defstruct learner
  (id (funcall *learner-id-generator*))
  (program (make-program))
  (action (random-action)))

(defun reference-p (form)
  "A reference is a list (GOTO <team-id>) where <team-id> is a symbol or string."
  (and (consp form) (eq (first form) 'GOTO) (second form)))

(defun atomic-p (learner)
  "A learner is atomic if its action is not a reference to another team."
  (not (reference-p (learner-action learner))))

(defun register-zero (registers)
  (elt registers 0))

(defun get-bid (learner observations vm-cache)
  (unless (typep observations '(simple-array double-float (*)))
    (error "'get-bid' received ~A.~%Expected (SIMPLE-ARRAY DOUBLE-FLOAT (*))." (type-of observations)))
  (let* ((id (learner-id learner))
         (vm-program (or (gethash id vm-cache)
                         (setf (gethash id vm-cache)
                               (build-vm-program (program-instructions (learner-program learner)))))))
    (let ((regs (execute-vm-program vm-program observations)))
      (aref regs 0))))
  
(defun random-action ()
  (let ((actions (experiment-actions *experiment*)))
    (random-choice actions)))

(defun get-reference (reference)
  (if (reference-p reference)
      (cadr reference)
      (error "Tried to get a reference of something that is not a reference. ~A~%" reference)))
