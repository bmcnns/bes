(in-package :bes)

(defun shuffle (list)
  "Return a shuffled copy of LIST using Fisher–Yates algorithm."
  (let* ((vec (coerce list 'vector))
         (n (length vec)))
    (loop for i from (1- n) downto 1
          do (rotatef (aref vec i)
                      (aref vec (random (1+ i)))))
    (coerce vec 'list)))

(defun seek-shuffle (seq)
  (let* ((cursor (random (length seq)))
         (before (subseq seq 0 cursor))
         (after (subseq seq cursor (length seq))))
    (reduce #'append (list after before))))

(defun choose (list)
  (let ((elt (random-choice list)))
    (values elt (remove elt list))))

(defun observation (step)
  (car (observations (list step))))

(defun action (step)
  (car (actions (list step))))

(defun lexicase-selection (tpg pool remaining-cases &optional (cases-so-far '()))
  (cond ((equal (length pool) 1)
         (values (first pool) (length cases-so-far)))
        ((null remaining-cases)
         (values (random-choice pool) (length cases-so-far)))
        (t
         (multiple-value-bind (next-case remaining-cases) (choose remaining-cases)
           (let ((new-pool (remove-if-not
                        (lambda (root-team)
                          (equal (execute-team tpg (team-id root-team) (observation next-case))
                                 (action next-case)))
                        pool)))
             (push next-case cases-so-far)
             (if (null new-pool)
                 (lexicase-selection tpg pool remaining-cases cases-so-far)
                 (lexicase-selection tpg new-pool remaining-cases cases-so-far)))))))

;; relatively effective generational model

    ;; (loop while (< (length offspring-ids) pop-size)
    ;;       do (let* ((parent (random-choice parents)))
    ;;            (multiple-value-bind (new-team new-learner) (mutate-team model parent :learner-table learner-table :team-table team-table)
    ;;              (when new-learner
    ;;                (setf combined (add-learners-to-tpg combined (list new-learner))))
    ;;              (when (not (equal (team-id new-team) (team-id parent)))
    ;;                (setf combined (add-teams-to-tpg combined (list new-team)))
    ;;                (push (team-id new-team) offspring-ids)))))
    ;; (let ((result (rebuild-tpg combined offspring-ids)))
    ;;   (loop while (> (length (teams result)) pop-size)
    ;;         do (let ((dropped-team (random-choice offspring-ids)))
    ;;              (if dropped-team
    ;;                  (progn (setf offspring-ids (remove dropped-team offspring-ids))
    ;;                         (setf result (rebuild-tpg combined offspring-ids)))
    ;;                  (return-from lexicase result))))
    ;;   result)))

