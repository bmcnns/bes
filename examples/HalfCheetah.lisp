(ql:quickload :bes)
(in-package :bes)

(defdataset *Minimal-HalfCheetah-Expert-v5*
  :path "~/.datasets/Minimal-HalfCheetah-Expert-v5")

(defdataset *HalfCheetah-Expert-v5*
  :path "~/.datasets/HalfCheetah-Expert-v5")

(defexperiment *HalfCheetah-v5*
  :batch-size 1000
  :instruction-set (ADD SUB MUL DIV SIN COS LOG EXP)
  :registers (R from 1 to 14) 
  :observations (OBS from 1 to 17)
  :output-registers (R from 1 to 6)
  :constant-range '(-10.0 10.0)
  :objectives (mean-squared-error)
  :tournament-size 4
  :num-threads 8
  :population-size 1000
  :generations 1000
  :minimum-program-length 8
  :maximum-program-length 128
  :observation-probability 0.5
  :constant-probability 0.5
  :mutate-instruction-probability 1.0
  :mutate-register-probability 0.5
  :mutate-operation-probability 0.25
  :mutate-constant-probability 0.25
  :add-instruction-probability 1.0
  :delete-instruction-probability 1.0
  :swap-instruction-probability 1.0
  :constant-mutation-std 1.0
  :maximum-instruction-count 256)

(defparameter *best-individuals* (make-hash-table))

(defun store-best-individual (trial-id genotype)
  (setf (gethash trial-id *best-individuals*) genotype))

(defun get-best-individual (trial-id)
  (gethash trial-id *best-individuals*))

(defun save-population (population trial save-path)
  (with-open-file (stream (format nil "~A/trial-~A.lisp" save-path trial)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (loop for genotype in population
          do (format stream "~A~%" genotype))))

(loop for trial from 1 to 10
      do 
         (format t "Starting trial ~A/~A...~%" trial 10)
         (ensure-directories-exist "runs/Minimal-HalfCheetah-Expert-v5/")
         (let* ((solutions (evolve *HalfCheetah-v5* *Minimal-HalfCheetah-Expert-v5*))
                (best-individual (car (argmin solutions #'cadr))))
           (save-population (mapcar #'car solutions) trial "runs/Minimal-HalfCheetah-Expert-v5")
           (store-best-individual trial best-individual))
         (rename-file "results.csv" (format nil "runs/Minimal-HalfCheetah-Expert-v5/results-~A.csv" trial)))

(loop for trial from 1 to 10
      do 
         (format t "Starting trial ~A/~A...~%" trial 10)
         (ensure-directories-exist "runs/HalfCheetah-Expert-v5/")
         (let* ((solutions (evolve *HalfCheetah-v5* *HalfCheetah-Expert-v5*))
                (best-individual (car (argmin solutions #'cadr))))
           (save-population (mapcar #'car solutions) trial "runs/HalfCheetah-Expert-v5")
           (store-best-individual trial best-individual))
         (rename-file "results.csv" (format nil "runs/HalfCheetah-Expert-v5/results-~A.csv" trial)))
