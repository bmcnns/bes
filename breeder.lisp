(in-package :bes)

(defun select-top-R (fitness-scores &key (R 0.20) (key #'first-objective) (pred #'<))
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


(defun breeder (eval-fn model log-fn)
  "Thread-safe breeder with delayed lucky-break spending (Option B / Python-style flywheel).
   Returns the next-generation model (TPG or LINEAR-GP)."
  (let* ((population-size (experiment-population-size *experiment*))
         ;; 1. Evaluate current population
         (scores (funcall eval-fn model)))

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
      (funcall log-fn scores)

      ;; --- 7. Fill population back to full size with mutated offspring ---
      (let ((new-model (fill-N-offspring model-after-selection parents gap)))
        ;; --- 8. Return new model (tail call friendly) ---
       new-model))))

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
