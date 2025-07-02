(ql:quickload :lparallel)
(load "~/Repos/bes/utils.lisp")
(load "~/Repos/bes/genotype.lisp")
(load "~/Repos/bes/macros.lisp")
(load "~/Repos/bes/instructions.lisp")
(load "~/Repos/bes/dataset.lisp")
(load "~/Repos/bes/hopper.lisp")
(load "~/Repos/bes/experiments.lisp")
(load "~/Repos/bes/phenotype.lisp")
(load "~/Repos/bes/mutation.lisp")
(load "~/Repos/bes/mutation.tests.lisp")
(load "~/Repos/bes/fitness.lisp")
(load "~/Repos/bes/selection.lisp")

(defun evolutionary-loop (experiment dataset population generation)
  (if (>= generation (experiment-generations experiment))
      ;; Base case: Generation count exceeded, return final population
      population
      ;; Recursive case: Evolutionary loop
      (let* ((batch (sample dataset experiment))
             (obs (observations batch))
             (acts (actions batch))
             (num-threads (experiment-num-threads experiment))
             (next-population
               (let ((ranked-population
                       (with-population population num-threads
                         (let ((predictions (phenotype individual experiment obs)))
                           (fitness individual #'negative-mean-squared-error acts predictions)))))
                 (-> ranked-population
                     (select experiment)
                     (with-population num-threads
                       (mutate individual experiment))))))
        (format t "Generation ~A complete.~%" (1+ generation))
        (force-output)
        (evolutionary-loop experiment dataset next-population (1+ generation)))))

(defun evolve (experiment dataset)
  (let* ((population-size (experiment-population-size experiment))
         (initial-population (loop repeat population-size
                                   collect (new-individual experiment))))
    (evolutionary-loop experiment dataset initial-population 0)))

      
