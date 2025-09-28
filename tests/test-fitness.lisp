(in-package :bes)

(deftest mean-squared-error-is-correct-result ()
  "Ensure that MEAN-SQUARED-ERROR gives the correct result."
  (and
    (assert-equal (mean-squared-error '(1 2 3) '(1 2 3)) 0)
    (assert-equal (mean-squared-error '(1 2 3) '(1 2 4)) (/ 1 3))
    (assert-equal (mean-squared-error '(1 2 3) '(2 3 4)) 1)
    (assert-equal (mean-squared-error '(1 2 3) '(0 1 2)) 1)
    (assert-equal (mean-squared-error '(1.0 2.0 3.0) '(1.5 2.5 3.5)) 0.25)))
