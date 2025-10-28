(in-package :bes)

(defparameter *team-id-generator* (make-unique-id-generator "T"))

(defun team-p (form)
  (and
   (listp form)
   (equal (car form) 'TEAM)
   (every #'symbolp (cddr form))))

(defun team-id (team)
  (unless (team-p team)
    (error "TEAM-ID expects a TEAM. Got ~A instead.~%" team))
  (cadr team))

(defun team-learners (team)
  (unless (team-p team)
    (error "TEAM-LEARNERS expects a TEAM. Got ~A instead.~%" team))
  (cddr team))

(defun team-count-unique-atomic-actions (tpg team &key (learner-table (build-learner-table tpg)))
  (let* ((learner-ids (team-learners team))
         (learners (mapcar (lambda (lid) (gethash lid learner-table)) learner-ids))
         (atomic-learners (remove-if-not #'atomic-p learners))
         (unique-atomic-actions (remove-duplicates (mapcar #'learner-action atomic-learners) :test #'equal)))
    (length unique-atomic-actions)))

(defun follow-reference (tpg reference observation &key
                                                     (visited nil)
                                                     (execution-path nil)
                                                     (learner-table (build-learner-table tpg))
                                                     (team-table (build-team-table tpg)))
  (cond ((member reference visited) (error "Cycle detected during team execution."))
        ((team-p reference) (execute-team tpg (team-id reference) observation :visited visited :learner-table learner-table :team-table team-table :execution-path execution-path))
        ((learner-p reference) (let* ((goto-team-id (get-reference (learner-action reference)))
                                      (goto-team (gethash goto-team-id team-table)))
                                 (if (null goto-team)
                                     (error "Learner pointed to ~A. Does this really exist?~%" goto-team-id))
                                 (push (learner-id reference) execution-path)
                                 (push goto-team-id execution-path)
                                 (push goto-team-id visited)
                                 (follow-reference tpg goto-team observation :visited visited :learner-table learner-table :team-table team-table :execution-path execution-path)))
        (t (error "Unexpected route while following reference. ~A~%" reference))))

(defun suggest-action (learner observation)
  (let ((action (learner-action learner)))
    (if (program-p action)
        (progn
          (execute-program action observation))
        action)))

(defun execute-team (tpg team-id observation &key
                                               (visited nil)
                                               (execution-path (list team-id))
                                               (learner-table (build-learner-table tpg))
                                               (team-table (build-team-table tpg)))
  (let* ((team (gethash team-id team-table))
         (learner-ids (team-learners team))
         (learners (mapcar (lambda (lid) (gethash lid learner-table)) learner-ids))
         (bids (mapcar (lambda (learner) (get-bid learner observation)) learners)))
    (let ((highest-bidder (elt learners (argmax bids))))
      (if (atomic-p highest-bidder)
          (values (suggest-action highest-bidder observation) (nreverse (push (learner-id highest-bidder) execution-path)))
          (follow-reference tpg highest-bidder observation :visited visited :learner-table learner-table :team-table team-table :execution-path execution-path)))))

(defun make-team (&key (attempts 5))
  (let* ((min (experiment-initial-minimum-number-of-learners *experiment*))
         (max (experiment-initial-maximum-number-of-learners *experiment*))
         (learners (loop repeat (random-range min max)
                         collect (make-learner)))
         (atomic-learners (remove-if-not #'atomic-p learners))
         (unique-atomic-actions (remove-duplicates (mapcar #'learner-action atomic-learners) :test #'equal))
         (team `(TEAM ,(funcall *team-id-generator*)
                      ,@(mapcar #'learner-id learners))))
    (cond
      ((>= (length unique-atomic-actions) 2)
       (values team learners))
      ((> attempts 0)
       (make-team :attempts (1- attempts)))
      (t
       (error "Ran out of attempts to make a team. Not enough distinct atomic actions to ensure >= 2 per team.")))))

(defun eval-team (team tpg dataset &key (learner-table (build-learner-table tpg)) (team-table (build-team-table tpg)))
  (let* ((team-id (team-id team))
         (observations (observations dataset))
         (actions (actions dataset))
         (predictions (mapcar (lambda (obs) (execute-team tpg team-id obs :learner-table learner-table :team-table team-table)) observations)))
    (fitness (cons tpg team) actions predictions learner-table team-table)))

(defun team-complexity (tpg team-id &key (learner-table (build-learner-table tpg)) (team-table (build-team-table tpg)))
  "A TEAM's complexity is the sum of the complexity of all its learners"
  (let* ((team (find-team-by-id tpg team-id :team-table team-table))
         (learners (team-learners team)))
    (apply '+ (mapcar (lambda (learner-id)
                        (learner-complexity tpg learner-id
                                            :learner-table learner-table
                                            :team-table team-table))
                      learners))))
 
(defun team-references (tpg team-id &key
                                      (team-table (build-team-table tpg))
                                      (learner-table (build-learner-table tpg)))
  "Returns the TEAMS that this TEAM'S learners point to."
  (let ((team (find-team-by-id tpg team-id :team-table team-table)))
    (loop for learner-id in (team-learners team)
          for learner = (find-learner-by-id tpg learner-id :learner-table learner-table)
          unless (atomic-p learner)
            collect (get-reference (learner-action learner)))))
        
