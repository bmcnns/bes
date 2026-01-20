(in-package :bes)

(defstruct vm-instruction
  (op :NO-OP :type keyword)
  (dest 0 :type fixnum)
  (src1-type :const :type keyword)
  (src1-val 0.0d0 :type double-float)
  (src2-type :const :type keyword)
  (src2-val 0.0d0 :type double-float))

(defstruct vm-program
  (code #() :type simple-vector))

(defun parse-operand (sym)
  "Helper to convert symbols like R1 or OBS5 into types and indices."
  (if (numberp sym)
      (values :const (coerce sym 'double-float))
      (let ((name (symbol-name sym)))
        (cond
          ((string= (subseq name 0 1) "R")
           (values :reg (coerce (1- (parse-integer (subseq name 1))) 'double-float)))
          ((string= (subseq name 0 3) "OBS")
           (values :obs (coerce (1- (parse-integer (subseq name 3))) 'double-float)))
          (t (error "Unknown operand: ~A" sym))))))

(defun build-vm-program (genotype)
  "Converts a list of symbols into VM-ready code"
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list genotype))
  (let ((instructions (make-array (length genotype))))
    (loop for raw-instr in genotype
          for i fixnum from 0
          do (destructuring-bind (dest op arg1 &optional arg2) raw-instr
               (declare (type symbol op))
               (multiple-value-bind (d-type d-val) (parse-operand dest)
                 (declare (ignore d-type) (type double-float d-val))
                 (multiple-value-bind (t1 v1) (parse-operand arg1)
                   (declare (type double-float v1))
                   (multiple-value-bind (t2 v2) (if arg2 (parse-operand arg2) (values :const 0.0d0))
                     (declare (type double-float v2))
                     (setf (aref instructions i)
                           (make-vm-instruction :op (intern (string op) :keyword)
                                                :dest (floor d-val)
                                                :src1-type t1
                                                :src1-val v1
                                                :src2-type t2
                                                :src2-val v2)))))))
    (make-vm-program :code instructions)))

(defun execute-vm-program (vm-prog observations)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (*)) observations))
  ;; Allocate 8 registers on the stack
  (let ((regs (make-array 8 :element-type 'double-float :initial-element 0.0d0))
        (code (vm-program-code vm-prog)))

    (loop for instr across code do
      (let ((op (vm-instruction-op instr))
            (dest (vm-instruction-dest instr))
            ;; Resolve Source 1
            (v1 (let ((val (vm-instruction-src1-val instr)))
                  (case (vm-instruction-src1-type instr)
                    (:reg (aref regs (truncate val)))
                    (:obs (aref observations (truncate val)))
                    (t val))))
            ;; Resolve Source 2
            (v2 (let ((val (vm-instruction-src2-val instr)))
                  (case (vm-instruction-src2-type instr)
                    (:reg (aref regs (truncate val)))
                    (:obs (aref observations (truncate val)))
                    (t val)))))
        (declare (type double-float v1 v2) (type fixnum dest))
        
        ;; Execute The Math
        (setf (aref regs dest)
              (case op
                (:ADD (+ v1 v2))
                (:SUB (- v1 v2))
                (:MULT (* v1 v2))
                (:DIV (if (zerop v2) 0.0d0 (/ v1 v2)))
                (:SIN (sin v1))
                (:COS (cos v1))
                (:NEG (- v1))
                (:LOG (if (> v1 0.0d0) (log v1) 0.0d0))
                (t 0.0d0)))))

    ;; Returns the regs vector
    regs))
                 
            


  
