(in-package :bes)

(defstruct learner
  (id (gensym "LEARNER-"))
  (program (make-program))
  (action (make-action)))

(defun bid (learner observations)
  "Return the first register after executing the learner's program.
   The first register indicates how confident the learner is in its action."
  (let ((program (learner-program learner)))
    (aref (execute-program program observations) 0)))

(defun clone-learner (learner)
  "Deep copy a learner."
  (make-learner
   :program (clone-program (learner-program learner))
   :action (clone-action (learner-action learner))))
