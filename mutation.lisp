(in-package :bes)

;; The micro-mutations

(defun mutate-dest (dest experiment)
  (let* ((registers (experiment-registers experiment)))
    (random-choice (remove dest registers))))

(defun mutate-operator (operator experiment)
  (let* ((arity (lookup-arity operator))
         (compatible-operators (instructions-with-arity arity)))
    (random-choice (remove operator compatible-operators))))

(defun mutate-operand (operand experiment)
  (let ((constant-probability (experiment-constant-probability experiment))
        (constant-range (experiment-constant-range experiment)))
    
    (if (weighted-coin-flip constant-probability)
        (random-range (first constant-range) (first (last constant-range)))
        (random-choice (remove operand `(,@(experiment-observations experiment) ,@(experiment-registers experiment)))))))


;; The macro-mutations
  
(defun swap-instructions (genotype)
  (let* ((i (random (length genotype)))
        (j (loop for r = (random (length genotype))
                 until (/= r i)
                 finally (return r))))
    (loop for instr in genotype
          for idx from 0
          collect (cond
                    ((= idx i) (nth j genotype))
                    ((= idx j) (nth i genotype))
                    (t instr)))))

(defun add-instruction (genotype experiment)
  (let* ((i (random (1+ (length genotype))))
         (new-instr (random-instruction experiment)))
    (loop for idx from 0
          for instr in genotype
          appending (if (= idx i)
                        (list new-instr instr)
                        (list instr))
          finally (when (= i (length genotype))
                    (return (append genotype (list new-instr)))))))

(defun remove-instruction (genotype)
  (let* ((i (random (length genotype))))
    (loop for instr in genotype
          for idx from 0
          when (/= idx i)
            collect instr)))

(defun mutate-constant (genotype experiment)
  (let* ((constant-locations (loop for instr in genotype
                                   for instr-idx from 0
                                   append (loop for arg in (cddr instr)
                                                for arg-idx from 2
                                                when (numberp arg)
                                                  collect (cons instr-idx arg-idx))))
         (target (when constant-locations (random-choice constant-locations))))
    (if (null target)
        genotype
        (let* ((std (experiment-constant-mutation-std experiment))
               (instr-idx (car target))
               (arg-idx (cdr target))
               (old-instr (nth instr-idx genotype))
               (old-const (nth arg-idx old-instr))
               (new-const (normal old-const std))
               (new-instr
                 (loop for x in old-instr
                       for i from 0
                       collect (if (= i arg-idx) new-const x))))
          (loop for instr in genotype
                for i from 0
                collect (if (= i instr-idx) new-instr instr))))))

(defun mutate-instruction (genotype experiment)
  (let* ((choice-of-mutation (random-choice '(destination operator args)))
         (target (random (length genotype)))
         (old-instruction (nth target genotype))
         (new-instruction
           (cond ((equal choice-of-mutation 'destination)
                  (loop for x in old-instruction
                        for i from 0
                        collect (if (= i 0) (mutate-dest x experiment) x)))

                 ((equal choice-of-mutation 'operator)
                  (loop for x in old-instruction
                        for i from 0
                        collect (if (= i 1) (mutate-operator x experiment) x)))

                 ((equal choice-of-mutation 'args)
                  (let* ((arg-index (random-range 2 (length old-instruction)))
                         (old-arg (nth arg-index old-instruction))
                         (new-arg (mutate-operand old-arg experiment)))
                    (loop for x in old-instruction
                          for i from 0
                          collect (if (= i arg-index) new-arg x))))

                 (t (error "Unknown form of mutation ~A~%" choice-of-mutation)))))
         (loop for instruction in genotype
               for i from 0
               collect (if (= i target) new-instruction instruction))))

;; The 'maybe' methods that are responsible for constraints and randomness

(defun maybe-add-instruction (genotype experiment)
  (let ((add-instruction-probability (experiment-add-instruction-probability experiment))
        (maximum-instruction-count (experiment-maximum-instruction-count experiment)))
    (if (and (bernoulli add-instruction-probability) (< (length genotype) maximum-instruction-count))
        (add-instruction genotype experiment)
        genotype)))

(defun maybe-remove-instruction (genotype experiment)
  (let ((delete-instruction-probability (experiment-delete-instruction-probability experiment)))
    (if (and (bernoulli delete-instruction-probability) (> (length genotype) 1))
        (remove-instruction genotype)
        genotype)))

(defun maybe-swap-instructions (genotype experiment)
  (let ((swap-instruction-probability (experiment-swap-instruction-probability experiment)))
    (if (and (bernoulli swap-instruction-probability) (> (length genotype) 1))
        (swap-instructions genotype)
        genotype)))

(defun maybe-mutate-instruction (genotype experiment)
  (let ((mutate-instruction-probability (experiment-mutate-instruction-probability experiment)))
    (if (bernoulli mutate-instruction-probability)
        (mutate-instruction genotype experiment)
        genotype)))

(defun maybe-mutate-constant (genotype experiment)
  (let ((mutate-constant-probability (experiment-mutate-constant-probability experiment)))
    (if (bernoulli mutate-constant-probability)
        (mutate-constant genotype experiment)
        genotype)))
    
                                        ; A Stephen Kelly approach to Mutation

(defun mutate (genotype experiment)
  (-> genotype
      (maybe-add-instruction experiment)
      (maybe-remove-instruction experiment)
      (maybe-swap-instructions experiment)
      (maybe-mutate-instruction experiment)
      (maybe-mutate-constant experiment)))
                               
