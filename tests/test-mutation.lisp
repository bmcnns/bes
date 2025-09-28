(in-package :bes)

(deftest never-delete-the-last-instruction ()
  "Confirm that the last instruction will never be removed when
   mutating a program."
  (assert-true 
    (let ((number-of-trials 10)
          (genotype '((R1 ADD R3 R4))))
      (loop for n from 1 to number-of-trials
            always (equal genotype (maybe-remove-instruction genotype))))))


(deftest never-exceed-maximum-instruction-count ()
  "Confirm that no instructions will be added when the program
   reaches its maximum size."
  (assert-true
   (let* ((number-of-trials 1000)
          (almost-maximum-instructions (loop for i from 0 below (experiment-maximum-instruction-count *experiment*)
                                             collect (random-instruction))))
     (loop for n from 1 to number-of-trials
           always (equal almost-maximum-instructions (maybe-add-instruction almost-maximum-instructions))))))

(deftest add-instruction-matches-frequency ()
  "Confirm that adding an instruction to a program through mutation
   matches the probability specified in *EXPERIMENT*."
  (let ((add-probability (experiment-add-instruction-probability *experiment*))
        (trials 1000)
        (epsilon 0.05))
    (assert-frequency trials add-probability epsilon
                      (= (length (maybe-add-instruction '((R1 ADD R2 R3)))) 2))))

(deftest remove-instruction-matches-frequency ()
  "Confirm that deleting an instruction from a program through mutation
   matches the probability specified in *EXPERIMENT*."
  (let ((remove-probability (experiment-remove-instruction-probability *experiment*))
        (trials 1000)
        (epsilon 0.05))
    (assert-frequency trials remove-probability epsilon
                      (= (length (maybe-remove-instruction '((R1 ADD R2 R3) (R2 COS R3)))) 1))))

(deftest swap-instruction-matches-frequency ()
  "Confirm that swapping instructions within a program through mutation
   matches the probability specified in *EXPERIMENT*."
  (let ((swap-probability (experiment-swap-instruction-probability *experiment*))
        (trials 1000)
        (epsilon 0.05))
    (assert-frequency trials swap-probability epsilon 
                      (equal (maybe-swap-instructions '((R1 ADD R2 R3) (R2 COS R3))) '((R2 COS R3) (R1 ADD R2 R3))))))

(deftest mutate-instruction-matches-frequency ()
  "Confirm that mutating instructions happens at the frequency
   specified in *EXPERIMENT*."
  (let ((mutate-probability (experiment-mutate-instruction-probability *experiment*))
        (trials 1000)
        (epsilon 0.05))
    (assert-frequency trials mutate-probability epsilon
                      (not (equal (maybe-mutate-instruction '((R1 ADD R2 R3) (R2 ADD R5 R6))) '((R1 ADD R2 R3) (R2 ADD R5 R6)))))))

(deftest mutate-instruction-always-different ()
  "Confirm that when you mutate an instruction, it will always be different afterwards.
   In other words, no no-ops."
  (assert-always 10
    (let* ((arbitrary-instruction (random-instruction))
           (genotype (list arbitrary-instruction)))
      (not (equal (mutate-instruction genotype) genotype)))))

(deftest mutate-dest-always-different ()
  (let ((trials 1000))
    (assert-always trials
                   (not (equal (mutate-dest 'R5) 'R5)))))

(deftest mutate-dest-always-register ()
  "Test that the DESTINATION register always remains an INTERNAL REGISTER
   during mutation. As opposed to becoming an OBSERVATION or constant."
  (let ((trials 1000))
    (assert-always trials
                   (member (mutate-dest 'R1) (experiment-registers *experiment*)))))

(deftest mutate-opcode-always-different ()
  "Test that the OPCODE is always changed during mutation."
  (let ((trials 1000))
    (assert-always trials
                   (let ((arbitrary-opcode (random-choice (experiment-instruction-set *experiment*))))
                     (not (equal (mutate-opcode arbitrary-opcode) arbitrary-opcode))))))

(deftest mutated-opcode-is-correct-arity ()
  "Confirm that when an OPCODE is mutated that its arity remains the same."
  (let ((trials 1000))
    (assert-always trials
                   (let* ((arbitrary-instruction (random-instruction))
                          (arbitrary-opcode (nth 1 arbitrary-instruction)))
                     (= (lookup-arity arbitrary-opcode) (- (length arbitrary-instruction) 2))))))

(deftest mutate-argument-matches-constant-probability ()
  "Test that the ARGUMENTS are sometimes CONSTANTs according to the
   *EXPERIMENT*'s constant probability."
  (let ((constant-probability (experiment-constant-probability *experiment*))
        (trials 1000)
        (epsilon 0.05))
    (assert-frequency trials constant-probability epsilon
                      (let* ((arbitrary-instruction (random-instruction))
                             (arbitrary-argument (random-choice (cddr arbitrary-instruction))))
                        (numberp (mutate-argument arbitrary-argument))))))

(deftest mutate-argument-always-different ()
  "Test that the ARGUMENTS are always changed during mutation.
   ARGUMENTS can either be REGISTERS or OBSERVATIONS."
  (let ((trials 1000))
    (assert-always trials
                   (let* ((arbitrary-instruction (random-instruction))
                          (arbitrary-argument (random-choice (cddr arbitrary-instruction))))
                     (not (equal (mutate-argument arbitrary-argument) arbitrary-argument))))))
