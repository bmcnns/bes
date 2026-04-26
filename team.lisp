(in-package :bes)

(defstruct (team (:constructor %make-team))
  (id (gensym "TEAM-"))
  (references 0) ;; Track how many learners point here.
  (type :root)
  (learners (loop repeat *init-num-learners*
		  collect (make-learner))))

(defun make-team (&rest args)
  "The primary team factory. Ensures every team is globally tracked."
  (let ((team (apply #'%make-team args)))
    (push team *teams*)
    team))

(defun add-reference (target-team)
  "Call this when a learner points to a team."
  (incf (team-references target-team))
  (setf (team-type target-team) :internal))

(defun delete-reference (target-team)
  "Call this when a learner is removed or mutated away from this team."
  (decf (team-references target-team))
  (when (<= (team-references target-team) 0)
    (setf (team-references target-team) 0)
    (setf (team-type target-team) :root)))

(defun execute-team (team observation)
  "Executes the TPG graph starting at TEAM.
   This follows the action of the learner with the highest bid."
  (let* ((learners (team-learners team))
	 (winner (alexandria:extremum learners #'>
				      :key (lambda (l) (bid l observation)))))
    (let ((act (learner-action winner)))
      (if (eq (action-type act) :atomic)
	  (action-action act)
	  (execute-team (action-action act) observation)))))

(defun execute-team-on-dataset (team dataset)
  "Batch executes a team across all the observations in DATASET."
  (map 'list (lambda (obs) (execute-team team obs)) (observations dataset)))

(defmethod print-object ((tm team) stream)
  "Updates the default printer to pretty print teams by showing
   whether they are root/internal and by enumerating their
   learner IDs, actions."
  (flet ((format-learner (learner)
	   (let ((id (learner-id learner))
		 (action (learner-action learner)))
	   (format nil "~A: ~A" id action))))
    (print-unreadable-object (tm stream :type nil :identity nil)
      (format stream "~A-TEAM ~A~%~{~A~%~}"
	      (team-type tm)
	      (team-id tm)
	      (mapcar #'format-learner (team-learners tm))))))

(defun closure (team)
  "Returns a list of all teams reachable from TEAM (including itself)."
  (let ((visited (make-hash-table :test 'eq)))
    (labels ((traverse (current)
	       (unless (gethash current visited)
		 (setf (gethash current visited) t)
		 (dolist (learner (team-learners current))
		   (let ((act (learner-action learner)))
		     (when (eq (action-type act) :reference)
		       (traverse (action-action act))))))))
      (traverse team))
    (alexandria:hash-table-keys visited)))

(defun creates-cycle-p (current-team target-team)
  "Returns T if the target-team has a path back to current-team."
  (member current-team (closure target-team) :test #'eq))

(defun root-teams ()
  "Returns all teams that are candidate solutions."
  (remove-if-not (lambda (team)
		   (eq (team-type team) :root))
		 *teams*))

(defun clone-team (team)
  "Deep copy a team."
  (make-team
   :type (team-type team)
   :learners (mapcar #'clone-learner (team-learners team))))

(defun delete-team (team)
  "Removes a team from the population and dereferences all of
   the internal teams it may reference."
  (setf *teams* (delete team *teams* :test #'eq))
  (dolist (learner (team-learners team))
    (let ((action (learner-action learner)))
      (when (eq (action-type action) :reference)
	(delete-reference (action-action action))))))
