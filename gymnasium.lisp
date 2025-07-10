(in-package :bes)

(setf py4cl:*python-command* "~/Repos/bes/.venv/bin/python3")
(py4cl:import-module "gymnasium" :as "gym")

(defun reset (environment seed)
  (coerce (car (py4cl:chain environment (reset :seed seed))) 'list))

(defun env-step (environment action)
  (let* ((state (py4cl:chain environment (step action)))
         (observation (coerce (car state) 'list))
         (reward (second state))
         (terminated (third state))
         (truncated (fourth state))
         (info (fifth state)))
    (values observation reward terminated truncated)))
  

(defun simulate (genotype environment-name seed experiment)
  (let* ((environment (gym:make environment-name))
         (episode-reward 0)
         (observation (reset environment seed)))
    (loop repeat 1000
          do (multiple-value-bind (obs rew term trunc) (env-step environment (phenotype genotype experiment observation))
               (if (or term trunc)
                   (return)
                   (progn
                     (setf episode-reward (+ episode-reward rew))
                     (setf observation obs)))))
  episode-reward))
