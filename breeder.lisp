(in-package :bes)

(defun select-top-R (fitness-scores &key (R 0.50) (key #'first-objective) (pred #'<))
  "Given a set of SCORES of (INDIVIDUAL-ID ((OBJ1 .. OBJ1-SCORE)..(OBJN .. OBJN-SCORE)))
   Return the top R PERCENTAGE of INDIVIDUALs using SCORES sorted by KEY.
   By default, the KEY is OBJ1-SCORE and the LOWEST VALUES are selected."
  (let ((n (floor (* R (length fitness-scores)))))
    (when (and (> R 0) (equal n 0))
      (incf n))
    (mapcar #'car (subseq (sort fitness-scores pred :key key) 0 n))))

(defun fill-N-offspring (model parents N)
  (cond
    ((tpg-p model)
     (let ((new-learners '())
           (new-teams '()))
       ;; fill remaining slots with offspring
       (loop while (< (length new-teams) N)
             do (let ((parent (random-choice parents)))
                  (multiple-value-bind (new-team new-learner)
                      (mutate-team model parent)
                    (when new-learner (push new-learner new-learners))
                    (unless (equal (team-id new-team) (team-id parent))
                      (push new-team new-teams)))))

       `(TPG (LEARNERS ,@(append (learners model) (nreverse new-learners)))
             (TEAMS ,@(append (teams model) (nreverse new-teams))))))))

(defun breeder (eval-fn model log-fn dataset)
  "Thread-safe breeder that preserves Elitism.
   Cycle: Evaluate -> Select -> Reproduce -> Vary (Offspring Only)."
  (let* ((population-size (experiment-population-size *experiment*))
         ;; 1. EVALUATE
         (scores (funcall eval-fn model)))

    ;; 2. SELECT (The Elites)
    (let* ((survivors (select #'select-top-R model scores))
           ;; Capture the IDs of the survivors so we know who NOT to touch
           (survivor-ids (mapcar (lambda (x) 
                                   (if (tpg-p model) (team-id x) (program-id x)))
                                 (if (tpg-p model) (teams survivors) (programs survivors))))
           
           ;; 3. REPRODUCE (Determine parents from survivors)
           (parents (cond ((tpg-p model)
                           (intersection (root-teams model)
                                         (root-teams survivors)
                                         :test #'equal))
                          ((linear-gp-p model)
                           (programs survivors))))
           
           (gap (cond ((tpg-p model)
                       (- population-size (length (teams survivors))))
                      ((linear-gp-p model)
                       (- population-size (length (programs survivors)))))))

      ;; Log stats based on the pure selection (before variation)
      (funcall log-fn scores)

      ;; 4. VARY (Fill gap with mutated offspring)
      (let ((next-gen-model (fill-N-offspring survivors parents gap)))
        
        ;; 5. TUNE CONSTANTS (Targeting ONLY the new offspring)
        ;; (when (tpg-p next-gen-model)
        ;;   (loop for team in (teams next-gen-model)
        ;;         ;; If this team was NOT in our survivor list, it is new. Tune it.
        ;;         unless (member (tem-id team) survivor-ids :test #'equal)
        ;;         do (tune-constants-in-team next-gen-model (team-id team) dataset)))
        
        ;; Return the model. 
        ;; The survivors are untouched, the offspring are mutated & tuned.
        next-gen-model))))

(defvar *num-datapoints-so-far* 0)

(defun breeder-with-data-tracking (eval-fn model log-fn)
  "Thread-safe breeder with delayed lucky-break spending (Option B / Python-style flywheel).
   Returns the next-generation model (TPG or LINEAR-GP)."
  (let* ((population-size (experiment-population-size *experiment*)))
    ;; 1. Evaluate current population
    (multiple-value-bind (scores num-datapoints)
        (funcall eval-fn model)

      (incf *num-datapoints-so-far* num-datapoints)

      ;; --- 3. Selection (spend breaks here if applicable) ---
      (let* ((model-after-selection (select #'select-top-R model scores))
             ;; 4. Determine parent pool
             (parents (cond ((tpg-p model)
                             (intersection (root-teams model)
                                           (root-teams model-after-selection)
                                           :test #'equal))
                            ((linear-gp-p model)
                             (programs model-after-selection))))
             ;; 5. Compute how many new individuals are needed
             (gap (cond ((tpg-p model)
                         (- population-size (length (teams model-after-selection))))
                        ((linear-gp-p model)
                         (- population-size (length (programs model-after-selection)))))))

        ;; --- 6. Log fitness statistics ---
        (funcall log-fn scores *num-datapoints-so-far*)

        ;; --- 7. Fill population back to full size with mutated offspring ---
        (let ((new-model (fill-N-offspring model-after-selection parents gap)))
          ;; --- 8. Return new model (tail call friendly) ---
          new-model)))))
