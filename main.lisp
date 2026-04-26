(in-package :bes)

(defun seed-or-random-seed (seed)
  "The start-search TCP packet will either contain :random or an integer seed.
   If an integer is provided, return it as is. If it is :random, return a random integer."
  (if (eq seed :random)
      (random 9999999999)
      seed))

(defun make-initial-population ()
  "Set the population to an initial set of random candidate solutions."
  (setf *teams* (loop repeat *population-size*
			   collect (make-team))))


(defun accuracy (team dataset)
  (let ((predictions (execute-team-on-dataset team dataset))
	(actuals (actions dataset)))
    (/ (loop for actual across actuals
	  for predicted in predictions
	     count (= actual predicted))
       (length actuals))))
	  
(defun make-fitness-function (&key gym-environment-name dataset-name)
  (cond
    (gym-environment-name (error "Online evaluation is not implemented yet."))
    (dataset-name
     (let ((dataset (load-dataset dataset-name)))
       (setf *fitness-fn* 
	     (lambda (team)
	       (accuracy team dataset)))))))
  
(defun safe-evaluate-team (team)
  (cons team
        (handler-case
            (funcall *fitness-fn* team)
          (floating-point-overflow () :bad)
          (floating-point-invalid-operation () :bad)
          (division-by-zero () :bad)
          (error () :bad))))

(defun evaluate ()
  "Returns a list of (team . fitness), skipping and deleting bad teams."
  (let* ((results (lparallel:pmapcar #'safe-evaluate-team
                                     (root-teams)))
         (bad-teams (loop for (team . fitness) in results
                          when (eq fitness :bad)
                            collect team))
         (good-results (remove :bad results :key #'cdr)))
    ;; Do mutation/deletion serially.
    (dolist (team bad-teams)
      (delete-team team))
    good-results))

(defun select (scores)
  "Remove GAP percent of the population by removing the worst teams."
  (let* ((sorted (sort (copy-list scores) #'> :key #'cdr))
	 (n-remove (floor (* *gap* (length scores))))
	 (worst (last sorted n-remove))
	 (best-fitness (cdr (first sorted))))
    (emit-fitness-scores (who-am-i) best-fitness *generation*)
    (dolist (entry worst)
      (delete-team (car entry)))))

(defun reproduce ()
  (loop while (< (length (root-teams)) *population-size*)
	do (mutate-team (clone-team (random-choice (root-teams))))))

(defun evolve ()
  "Evolve the population for a single generation."
  (select (evaluate))
  (reproduce))

(defun run-search (mode gym-environment-name dataset-name seed)
  "Search the solution space with a tangled program graph."
  (let* ((seed (seed-or-random-seed seed))
	 (captured-state (sb-ext:seed-random-state seed)))
    
    (setf *random-state* captured-state)

    (when *running*
      (emit-error "A search is already running on this node.")
      (return-from run-search))

    (setf *teams* nil)
    (setf *running* t)
    (setf *generation* 0)
    
    ;; enable multi-threading
    (setf lparallel:*kernel* (make-kernel +num-threads+))
    
    ;; make the initial population
    (make-initial-population)

    (ecase mode
      (:offline (make-fitness-function :dataset-name dataset-name)))

    (push (bt:make-thread
	   (lambda ()
	     (let ((*random-state* (make-random-state captured-state)))
	       (unwind-protect
		    (loop while *running*
			  do (evolve)
			  do (incf *generation*))
		 (lparallel:end-kernel))))
	   :name "search-thread")
	  *server-threads*)))

		   
					     
