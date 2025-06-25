# Bryce Evolutionary System (BES)

BES (Bryce Evolutionary System) is a **compiled Linear Genetic Programming (LGP)** system implemented in **Common Lisp**. Rather than interpreting evolved programs at runtime, BES compiles individuals into native Lisp functions for fast and efficient execution.

The system is influenced by:

- **Linear Genetic Programming** from Brameier & Banzhaf, emphasizing a register-based representation and effective use of introns.
- Borrowing ideas from Stephen Kelly's work on Tangled Program Graphs implementation of mutation at the program and instruction level.

All data structures and transformations in BES are **immutable** by design.

---

## Features

- **Compiled program execution**: Programs are turned into executable Lisp code rather than being interpreted.
- **LGP architecture**: Register-based instructions using operators like `ADD`, `SUB`, `MUL`, `DIV`, `SIN`, `COS`, etc.
- **Rich mutation suite**: Includes instruction replacement, operand mutation, insertion/removal, constant perturbation, and more — inspired by TPG mutation flexibility.
- **Macro-based DSL**: Define experiments and run evaluations using expressive Common Lisp macros.
- **Minimal unit test framework**: Custom `deftest` and `check` macros support lightweight testing.

---

## File Overview

```

bes/
├── experiments.lisp ; Define experimental configurations and hyperparameters
├── genotype.lisp ; Genotype (program) data structures
├── instructions.lisp ; Evaluation of instructions (compiled semantics)
├── macros.lisp ; Utility macros and DSL for tests/experiments
├── main.lisp ; Entrypoint and evolutionary loop
├── mutation.lisp ; All mutation operators (insert, delete, perturb, etc.)
├── mutation.tests.lisp ; Tests for mutation operators
├── phenotype.lisp ; Registers, execution, and program compilation
├── utils.lisp ; General-purpose helpers

```

---

## Quick Start

### Requirements

- [SBCL](http://www.sbcl.org/) or compatible Common Lisp implementation
- [Quicklisp](https://www.quicklisp.org/)
- [SLIME](https://common-lisp.net/project/slime/) for development (recommended)

### Running

```lisp
(load "main.lisp")

(defparameter *exp*
  (make-experiment
   :instruction-set (make-instruction-set 'ADD 'SUB 'MUL 'DIV)
   :registers '(R1 R2 R3 R4)
   :observations '(OBS1 OBS2 OBS3)
   :output-registers '(R1)
   :population-size 500
   :generations 50))

(run-experiment *exp*)
```

## References

Brameier, M., & Banzhaf, W. (2006). Linear Genetic Programming. Springer.

Kelly, S., & Heywood, M. I. (2018). Emergent Tangled Program Graphs in Multi-Task Learning. IJCAI.
