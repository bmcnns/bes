(defparameter *MOCK-EXPERIMENT*
  (make-experiment
   :instruction-set (make-instruction-set 'ADD 'SUB 'MUL 'DIV 'SIN 'COS 'LOG 'EXP)
   :registers '(R1 R2 R3 R4 R5 R6 R7 R8 R9 R10)
   :observations '(OBS1 OBS2 OBS3 OBS4 OBS5 OBS6)
   :output-registers '(R1)
   :constant-range '(-10.0 10.0)
   :population-size 1000
   :generations 10
   :minimum-program-length 2
   :maximum-program-length 6
   :observation-probability 0.2
   :constant-probability 0.2
   :mutate-instruction-probability 1.0
   :mutate-register-probability 0.5
   :mutate-operation-probability 0.25
   :mutate-constant-probability 0.25
   :add-instruction-probability 0.5
   :delete-instruction-probability 0.5
   :swap-instruction-probability 0.2
   :constant-mutation-std 0.5
   :maximum-instruction-count 256))

(deftest test-never-delete-the-last-instruction ()
  (check 
    (let ((number-of-trials 10)
          (genotype '((R1 ADD R3 R4))))
      (loop for n from 1 to number-of-trials
            always (equal genotype (maybe-remove-instruction genotype *MOCK-EXPERIMENT*))))))

(deftest test-never-exceed-maximum-instruction-count ()
  (check
    (let* ((number-of-trials 1000)
          (almost-maximum-instructions (loop for i from 0 below (experiment-maximum-instruction-count *mock-experiment*)
                                             collect (new-instruction *mock-experiment*))))
      (loop for n from 1 to number-of-trials
            always (equal almost-maximum-instructions (maybe-add-instruction almost-maximum-instructions *mock-experiment*))))))

                                        ; Test macromutations

(deftest test-add-instruction-matches-frequency ()
  (check-frequency (1000 (experiment-add-instruction-probability *mock-experiment*) 0.05)
    (= (length (maybe-add-instruction '((R1 ADD R2 R3)) *mock-experiment*)) 2)))

(deftest test-delete-instruction-matches-frequency ()
  (check-frequency (1000 (experiment-delete-instruction-probability *mock-experiment*) 0.05)
    (= (length (maybe-remove-instruction '((R1 ADD R2 R3) (R2 COS R3)) *mock-experiment*)) 1)))

(deftest test-swap-instruction-matches-frequency ()
  (check-frequency (1000 (experiment-swap-instruction-probability *mock-experiment*) 0.05)
    (equal (maybe-swap-instructions '((R1 ADD R2 R3) (R2 COS R3)) *mock-experiment*) '((R2 COS R3) (R1 ADD R2 R3)))))

(deftest test-mutate-instruction-matches-frequency ()
  (check-frequency (1000 (experiment-mutate-instruction-probability *mock-experiment*) 0.05)
    (not (equal (maybe-mutate-instruction '((R1 ADD R2 R3) (R2 ADD R5 R6)) *mock-experiment*) '((R1 ADD R2 R3) (R2 ADD R5 R6))))))

(deftest test-mutate-instruction-always-different ()
  (check-frequency (1000 1 0)
    (let* ((arbitrary-instruction (new-instruction *mock-experiment*))
           (genotype `(,arbitrary-instruction)))
      (not (equal (mutate-instruction genotype *mock-experiment*) genotype)))))
  
                                        ; Test micromutations

;; test mutate-dest

(deftest test-mutate-dest-always-different ()
  (check-frequency (1000 1 0)
    (not (equal (mutate-dest 'R5 *mock-experiment*) 'R5))))

(deftest test-mutate-dest-always-register ()
  (check-frequency (1000 1 0)
    (member (mutate-dest 'R1 *mock-experiment*) (experiment-registers *mock-experiment*))))

;; test mutate-opcode

(deftest test-mutate-opcode-always-different ()
  (check-frequency (1000 1 0)
    (let ((arbitrary-opcode (random-choice (experiment-instruction-set *mock-experiment*))))
      (not (equal (mutate-opcode arbitrary-opcode *mock-experiment*) arbitrary-opcode)))))

(deftest test-mutated-opcode-is-correct-arity ()
  (check-frequency (1000 1 0)
    (let* ((arbitrary-instruction (new-instruction *mock-experiment*))
           (arbitrary-opcode (nth 1 arbitrary-instruction)))
      (= (lookup-arity arbitrary-opcode) (- (length arbitrary-instruction) 2)))))

;; test mutate-argument

(deftest test-mutate-argument-matches-constant-probability ()
  (check-frequency (1000 (experiment-constant-probability *mock-experiment*) 0.05)
    (let* ((arbitrary-instruction (new-instruction *mock-experiment*))
           (arbitrary-argument (random-choice (cddr arbitrary-instruction))))
    (numberp (mutate-argument arbitrary-argument *mock-experiment*)))))

(deftest test-mutate-argument-always-different ()
  (check-frequency (1000 1 0)
    (let* ((arbitrary-instruction (new-instruction *mock-experiment*))
           (arbitrary-argument (random-choice (cddr arbitrary-instruction))))
      (not (equal (mutate-argument arbitrary-argument *mock-experiment*) arbitrary-argument)))))

                                        ; Test suites

(deftest test-mutate-opcode ()
  (test-mutate-opcode-always-different)
  (test-mutated-opcode-is-correct-arity))

(deftest test-mutate-dest ()
  (test-mutate-dest-always-different)
  (test-mutate-dest-always-register))

(deftest test-mutate-argument ()
  (test-mutate-argument-matches-constant-probability)
  (test-mutate-argument-always-different))

(deftest test-micro-mutations ()
  (test-mutate-opcode)
  (test-mutate-dest)
  (test-mutate-argument))

(deftest test-macro-mutations ()
  (test-never-delete-the-last-instruction)
  (test-never-exceed-maximum-instruction-count)
  (test-add-instruction-matches-frequency)
  (test-delete-instruction-matches-frequency)
  (test-swap-instruction-matches-frequency)
  (test-mutate-instruction-matches-frequency))

(deftest test-mutations ()
  (test-macro-mutations)
  (test-micro-mutations))

