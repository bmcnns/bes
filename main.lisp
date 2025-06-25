(load "~/Repos/bes/utils.lisp")
(load "~/Repos/bes/genotype.lisp")
(load "~/Repos/bes/instructions.lisp")
(load "~/Repos/bes/experiments.lisp")
(load "~/Repos/bes/phenotype.lisp")
(load "~/Repos/bes/macros.lisp")
(load "~/Repos/bes/mutation.lisp")


(defun new-population (experiment)
  (loop repeat (experiment-population-size experiment)
        collect (new-individual experiment)))

(defun evolve (experiment)
  (let ((population (new-population experiment))
        (generations (experiment-generations experiment)))
    (print "Running search")
    (loop repeat generations
          do (print "New generation"))
    (print "Search complete")
    population))


