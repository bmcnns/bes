(in-package :bes)

(defun make-unique-id-generator (prefix)
  "Returns a closure that generates a unique ID
   Note: Not thread-safe."
  (let ((counter 0))
    (lambda ()
      (intern (format nil "~A~D" (string-upcase prefix) (incf counter))))))

(defparameter *program-id-generator* (make-unique-id-generator "P"))
(defparameter *team-id-generator* (make-unique-id-generator "T"))
(defparameter *learner-id-generator* (make-unique-id-generator "L"))

;; core tpg


;; cycle detection and prevention

(defun team-out-neighbours (team)
  "List of target team-ids this TEAM references via its learners' actions."
  (remove nil
          (loop for lr in (team-learners team)
                for ref = (%normalize-team-id (reference-target-id (learner-action lr)))
                collect ref)))

(defun build-adjacency ()
  "Return hash: team-id -> (list target-team-id ...). Ignores dangling refs."
  (let ((adj (make-hash-table :test 'equal)))
    (dolist (tid (all-team-ids))
      (let* ((tm (lookup-team tid))
             (targets (team-out-neighbours tm)))
        (setf (gethash tid adj) targets)))
    adj))

(defun reachable-p (from-id to-id &optional (adj (build-adjacency)))
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

(defun find-any-cycle (&optional (adj (build-adjacency)))
  "Return a list representing a cycle path (v0 v1 ... v0), or NIL if acyclic."
  (let ((color (make-hash-table :test 'equal)) ; nil/white, :gray, :black
        (parent (make-hash-table :test 'equal))
        (cycle nil))
    (labels ((visit (u)
               (setf (gethash u color) :gray)
               (dolist (v (gethash u adj))
                 (cond
                   ((null (gethash v color))
                    (setf (gethash v parent) u)
                    (when (visit v) (return-from visit t)))
                   ((eq (gethash v color) :gray)
                    ;; Found a back-edge u -> v; reconstruct cycle u...v->u
                    (let ((path (list u)))
                      (loop for x = u then (gethash x parent)
                            until (equal x v)
                            do (push (gethash x parent) path))
                      (setf cycle
                            (let ((p (nreverse path)))
                              (append p (list (first p)))))
                      (return-from visit t)))))
               (setf (gethash u color) :black)
               nil))
      (dolist (u (all-team-ids))
        (unless (gethash u color)
          (when (visit u) (return-from find-any-cycle cycle))))
      nil)))

(defun validate-acyclic-or-error ()
  "Signal a descriptive error if a cycle exists anywhere."
  (let ((c (nth-value 0 (find-any-cycle))))
    (when c
      (let* ((*print-circle* nil)) ; ensure no #n=/#n#
        (error (format nil "Cycle detected through teams: ~{~A~^ -> ~}" c))))))
