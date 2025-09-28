(in-package :bes)

(defun build-adjacency (tpg)
  "Return hash: team-id -> list target-team-id ... )."
  (let ((adj (make-hash-table :test 'equal))
        (teams (teams tpg)))
    (dolist (team teams)
      (let ((team-id (team-id team))
            (neighbours (team-neighbours tpg team)))
        (setf (gethash team-id adj) neighbours)))
    adj))

(defun reachable-p (tpg from-id to-id &optional (adj (build-adjacency tpg)))
  "Is there a path from FROM-ID to TO-ID in adjacency ADJ?"
  (let ((seen (make-hash-table :test 'equal))
        (stack (list from-id)))
    (loop while stack
          for v = (pop stack) do
            (when (equal v to-id) (return-from reachable-p t))
            (unless (gethash v seen)
              (setf (gethash v seen) t)
              (dolist (w (gethash v adj)) (push w stack))))
    nil))

(defun would-create-cycle-p (tpg from-id to-id &optional (adj (build-adjacency tpg)))
  "Adding edge FROM-ID -> TO-ID creates a cycle iff TO-ID can already reach FROM-ID,
   or TO-ID = FROM-ID (self-loop)."
  (or (equal from-id to-id)
      (reachable-p tpg to-id from-id adj)))
