(in-package :bes)

(defparameter *team-id-generator* (make-unique-id-generator "T"))

(defstruct (team (:constructor %make-team))
  (id (funcall *team-id-generator*))
  learners)
  
(defun execute-team (team observation vm-cache)
  (declare (optimize (speed 3) (safety 0))
           ;; Strict type for fast array access
           (type (simple-array double-float (*)) observation))
  
  (let ((learners (team-learners team))
        ;; Initialize best-bid to -Infinity so the first learner always updates it
        (best-bid sb-ext:double-float-negative-infinity) 
        (best-learner nil))
    (declare (type double-float best-bid))
    
    ;; 1. Find the winner in one pass (Zero Allocation)
    (dolist (learner learners)
      ;; We assume get-bid returns a float. 
      ;; If get-bid is allocating, we will see it in the next profile.
      (let ((bid (the double-float (get-bid learner observation vm-cache))))
        (when (> bid best-bid)
          (setf best-bid bid)
          (setf best-learner learner))))
    
    ;; 2. Execute the winner
    (let ((action (learner-action best-learner)))
      (if (team-p action)
          ;; Recursive Step: Returns a double-float from the bottom
          (execute-team action observation vm-cache)
          
          ;; Base Case: Atomic Action
          ;; Accuracy expects a float. If action is an integer (index), coerce it.
          ;; This coerce happens in a register (cvtsi2sd), no consing.
          action))))

(defun make-team (&key (attempts 5))
  (let* ((min (experiment-initial-minimum-number-of-learners *experiment*))
         (max (experiment-initial-maximum-number-of-learners *experiment*))
         (learners (loop repeat (random-range min max)
                         collect (make-learner)))
         (atomic-learners (remove-if-not #'atomic-p learners))
         (unique-atomic-actions (remove-duplicates (mapcar #'learner-action atomic-learners) :test #'equal)))
    (cond
      ((>= (length unique-atomic-actions) 2)
       (%make-team :id (funcall *team-id-generator*)
                  :learners learners))
      ((> attempts 0)
       (make-team :attempts (1- attempts)))
      (t
       (error "Ran out of attempts to make a team. Not enough distinct atomic actions to ensure >= 2 per team.")))))

(defun accuracy (team dataset vm-cache)
  (declare (optimize (speed 3) (safety 0)))
  (let ((X (observations dataset))
        (y (actions dataset))
        (correct 0))
    (declare (type fixnum correct)
             (type simple-vector X)
             ;; FIX: Match the struct definition (Generic Vector)
             (type simple-vector y)) 

    (loop for i fixnum from 0 below (length y)
          do (let* ((pred (the fixnum (execute-team team (aref X i) vm-cache)))
                    ;; FIX: Read the pointer, then promise it points to a float
                    (target (the fixnum (aref y i))))
               
               ;; This comparison is still fast (float registers)
               ;; but we pay a small cost to dereference the 'y' pointer first.
               (when (= pred target) 
                 (incf correct))))
    
    (- (/ (float correct 0d0) (length y)))))

(defun eval-team (team dataset)
  (accuracy team dataset (make-hash-table :test 'eql)))
         
