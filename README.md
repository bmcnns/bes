# Bryce Evolutionary System (BES)

BES (Bryce Evolutionary System) is a **compiled Linear Genetic Programming (LGP)** system implemented in **Common Lisp**. Rather than interpreting evolved programs at runtime, BES compiles individuals into native Lisp functions for fast and efficient execution.

The system is influenced by:

- **Linear Genetic Programming** from Brameier & Banzhaf, emphasizing a register-based representation and effective use of introns.
- Borrowing ideas from Stephen Kelly's work on Tangled Program Graphs implementation of mutation at the program and instruction level.

All data structures and transformations in BES are **immutable** by design.

---

## Features

- **Compiled programs** — Individuals are JIT-compiled into native machine code to speed up execution
- **Safe instructions** — Arithmetic, trigonometric, and protected functions.
- **Multi-objective support** — NSGA-II for minimizing error & complexity.
- **Parallel evaluation** — Uses `lparallel` for speed.
- **Python plotting** — Residuals, Pareto fronts via `matplotlib` and `py4cl`.

---

## File Overview

| File              | Purpose                                                  |
|-------------------|----------------------------------------------------------|
| `instructions.lisp` | Safe op definitions (e.g. `protected-div`, `cos`)        |
| `genotype.lisp`     | Program representation & random generation               |
| `mutation.lisp`     | Instruction + structural mutations                       |
| `evolution.lisp`    | Evolution loop (`evolve`)                                |
| `dataset.lisp`      | `defdataset` for loading data                            |
| `experiments.lisp`  | `defexperiment` macro for experiment configuration       |
| `phenotype.lisp`    | Compilation of programs to executable Lisp functions     |
| `fitness.lisp`      | Error and complexity objectives                          |
| `selection.lisp`    | Tournament & NSGA-II selection                           |
| `hopper.lisp`       | Offline RL example (Hopper dataset)                      |
| `utils.lisp`        | Helper functions                                         |
| `package.lisp`      | BES package definition and exports                       |
| `bes.asd`           | ASDF system definition                                   |

---

## Quick Start

### Requirements

- [SBCL](http://www.sbcl.org/) or compatible Common Lisp implementation
- [Quicklisp](https://www.quicklisp.org/)
- [SLIME](https://common-lisp.net/project/slime/) for development (recommended)

### Running

```lisp
(defdataset *Minimal-Hopper-Expert-v5*)

(defexperiment *Hopper-v5*
  :batch-size 1000
  :instruction-set (make-instruction-set 'ADD 'SUB 'MUL 'DIV 'SIN 'COS 'LOG 'EXP)
  :registers (symbols R from 1 to 11) 
  :observations (symbols OBS from 1 to 11)
  :output-registers (symbols R from 1 to 3)
  :constant-range '(-10.0 10.0)
  :objectives `((minimize MSE) (minimize complexity))
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

(evolve *Hopper-v5* *Minimal-Hopper-Expert-v5*)
```

## References

Brameier, M., & Banzhaf, W. (2006). Linear Genetic Programming. Springer.

Kelly, S., & Heywood, M. I. (2018). Emergent Tangled Program Graphs in Multi-Task Learning. IJCAI.
