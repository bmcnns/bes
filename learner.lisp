(in-package :bes)

(defparameter *learner-id-generator* (make-unique-id-generator "L"))

(defun learner-p (form)
  (and
   (listp form)
   (equal (car form) 'LEARNER)
   (program-p (caddr form))
   (and
    (cadddr form)
    (or
     (program-p (cadddr form))
     (listp (cadddr form))
     (symbolp (cadddr form))
     (numberp (cadddr form))))))

(defun learner-id (learner)
  (unless (learner-p learner)
    (error "LEARNER-ID expects a LEARNER. Got ~A instead.~%" learner))
  (cadr learner))

(defun learner-program (learner)
  (unless (learner-p learner)
    (error "LEARNER-PROGRAM expects a LEARNER. Got ~A instead.~%" learner))
  (caddr learner))

(defun learner-action (learner)
  (unless (learner-p learner)
    (error "LEARNER-ACTION expects a LEARNER. Got ~A instead.~%" learner))
  (cadddr learner))

(defun reference-p (form)
  "A reference is a list (GOTO <team-id>) where <team-id> is a symbol or string."
  (and (consp form) (eq (first form) 'GOTO) (second form)))

(defun atomic-p (learner)
  "A learner is atomic if its action is not a reference to another team."
  (not (reference-p (learner-action learner))))

(defun get-bid (learner observation)
  (let ((program (learner-program learner)))
    (elt (execute-program program observation) 0)))

(defun get-reference (reference)
  (if (reference-p reference)
      (cadr reference)
      (error "Tried to get a reference of something that is not a reference. ~A~%" reference)))

(defun make-learner ()
  (let ((actions (experiment-actions *experiment*)))
    `(LEARNER ,(funcall *learner-id-generator*) ,(make-program) ,(random-choice actions))))

