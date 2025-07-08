(in-package :bes)

;;; evolution.lisp
;;; -------------
;;;
;;; This file contains the core evolutionary loop logic for BES.
;;; It defines macros and functions for population initialization,
;;; parallel execution, and the main `evolve` function driving the
;;; generational updates. These macros are used to structure and
;;; schedule transformations over populations.


(defmacro with-population (population threads &body forms)
  "Execute FORMS on each individual in POPULATION in parallel.
   THREADS specifies how many threads to use."
  (let ((pop-var (gensym "POP")))
    `(let ((,pop-var ,population))
       (if (> ,threads 1)
           (progn
             (unless (find-package 'lparallel)
               (ql:quickload :lparallel))
             (setf lparallel:*kernel* (lparallel:make-kernel ,threads))
             (unwind-protect
                  (progn
                    ,@(loop for form in forms
                            collect
                            `(setf ,pop-var
                                   (lparallel:pmap 'list (lambda (individual) ,form) ,pop-var))))
               (lparallel:end-kernel :wait t)))
           (progn
             ,@(loop for form in forms
                     collect
                     `(setf ,pop-var
                            (mapcar (lambda (individual) ,form) ,pop-var)))))
       ,pop-var)))



(defmacro defpopulation (name size experiment)
  "Define a global population NAME as a list of SIZE genotypes.
   Each genotype is created using NEW-GENOTYPE with EXPERIMENT
   provided for initialization parameters."
  `(defparameter ,name (loop repeat ,size
                             collect (new-genotype ,experiment))))

(defun multi-objective-optimization (dataset population experiment)
  "Perform one generation of NSGA-II multi-objective optimization.
   Takes DATASET, current POPULATION, and EXPERIMENT configuration.
   Returns the next population using non-dominated sorting and crowding distance."
  (let* ((num-threads (experiment-num-threads experiment))
         (parents population)
         (observations (observations dataset))
         (actions (actions dataset))
         (children (with-population population num-threads
                     (mutate individual experiment)))
         (combined-population (append children parents))
         (ranked-population (with-population combined-population num-threads
                              (fitness experiment individual actions (phenotype individual experiment observations))))
         (pareto-fronts (non-dominated-sorting ranked-population))
         (ranked-next-population '()))
    (loop for front in pareto-fronts
          do (let ((remaining (- (length parents) (length ranked-next-population))))
               (cond
                 ((<= (length front) remaining)
                  (setf ranked-next-population (nconc ranked-next-population front)))
                 (t (let* ((sorted-by-crowding (sort-by-crowding-distance front))
                           (selected (subseq sorted-by-crowding 0 remaining)))
                      (setf ranked-next-population (nconc ranked-next-population selected)))
                    (return)))))
    (mapcar #'car ranked-next-population)))

(defun single-objective-optimization (dataset population experiment)
  "Perform one generation of single-objective-optimization using tournament selection.
   Takes DATASET, current POPULATION and EXPERIMENT configuration.
   Returns the next generation after selection and mutation."
  (let* ((num-threads (experiment-num-threads experiment))
         (observations (observations dataset))
         (actions (actions dataset))
         (ranked-population
           (with-population population num-threads
             (let ((predictions (phenotype individual experiment observations)))
               (fitness experiment individual actions predictions)))))
    (-> ranked-population
        (single-objective-selection experiment)
        (with-population num-threads
            (mutate individual experiment)))))
  

(defun evolutionary-loop (experiment dataset population generation &key evolution-strategy)
  "Run the evolutionary loop recursively for the given EXPERIMENT.
   Takes DATASET, initial POPULATION, current GENERATION, and EVOLUTION-STRATEGY.
   Terminates after a fixed number of generations. Returns final population."
  (if (>= generation (experiment-generations experiment))
      population
      (let* ((batch (sample dataset experiment))
             (num-threads (experiment-num-threads experiment))
             (next-population (funcall evolution-strategy batch population experiment)))
        (format t "Generation ~A complete.~%" (1+ generation))
        (force-output)
        (evolutionary-loop experiment dataset next-population (1+ generation) :evolution-strategy evolution-strategy))))

(defun evolve (experiment dataset)
  "Initialize a population and run the evolutionary loop using multi-objective optimization.
   Takes an EXPERIMENT definition and a DATASET.
   Returns the final evolved population after the specified number of generations in EXPERIMENT."
  (let* ((population-size (experiment-population-size experiment))
         (initial-population (loop repeat population-size
                                   collect (new-genotype experiment))))
    (if (> (length (experiment-objectives experiment)) 1)
        (evolutionary-loop experiment dataset initial-population 0 :evolution-strategy #'multi-objective-optimization)
        (evolutionary-loop experiment dataset initial-population 0 :evolution-strategy #'single-objective-optimization))))
