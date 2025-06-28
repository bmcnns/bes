(deftest test-mean-squared-error ()
  (check
    (= (mean-squared-error '(1 2 3) '(1 2 3)) 0)
    (= (mean-squared-error '(1 2 3) '(1 2 4)) (/ 1 3))
    (= (mean-squared-error '(1 2 3) '(2 3 4)) 1)
    (= (mean-squared-error '(1 2 3) '(0 1 2)) 1)
    (= (mean-squared-error '(1.0 2.0 3.0) '(1.5 2.5 3.5)) 0.25)))
