(in-package :bes)

(deftest dominates-p-is-correct
  (let ((scores '((P2001 ((LOSS . 0.50) (COMPLEXITY . 3)))
                  (P2002 ((LOSS . 0.60) (COMPLEXITY . 2)))
                  (P2003 ((LOSS . 0.55) (COMPLEXITY . 4)))
                  (P2004 ((LOSS . 0.45) (COMPLEXITY . 5)))
                  (P2005 ((LOSS . 0.70) (COMPLEXITY . 1))))))
    (check
     (assert-true
      (every (lambda (individual) (dominates-p scores individual 'P2003)) '(P2001)))
     (assert-false
      (every (lambda (individual) (dominates-p scores individual 'P2001)) '(P2001 P2002 P2004 P2005)))
     (assert-false
      (every (lambda (individual) (dominates-p scores individual 'P2002)) '(P2001 P2002 P2004 P2005)))
     (assert-false
      (every (lambda (individual) (dominates-p scores individual 'P2004)) '(P2001 P2002 P2004 P2005)))
     (assert-false
      (every (lambda (individual) (dominates-p scores individual 'P2005)) '(P2001 P2002 P2004 P2005))))))
                    
(deftest pareto-front-gives-correct-front
  (let ((scores '((P2001 ((LOSS . 0.50) (COMPLEXITY . 3)))
                  (P2002 ((LOSS . 0.60) (COMPLEXITY . 2)))
                  (P2003 ((LOSS . 0.55) (COMPLEXITY . 4)))
                  (P2004 ((LOSS . 0.45) (COMPLEXITY . 5)))
                  (P2005 ((LOSS . 0.70) (COMPLEXITY . 1))))))
    (assert-equal (pareto-front scores) '(P2001 P2002 P2004 P2005))))

(deftest non-dominated-sorting-returns-fronts-in-correct-order
  (let ((scores '((P3001 ((LOSS . 0.40) (COMPLEXITY . 5)))   
                  (P3002 ((LOSS . 0.55) (COMPLEXITY . 2)))   
                  (P3003 ((LOSS . 0.60) (COMPLEXITY . 4)))   
                  (P3004 ((LOSS . 0.45) (COMPLEXITY . 6)))   
                  (P3005 ((LOSS . 0.70) (COMPLEXITY . 1)))   
                  (P3006 ((LOSS . 0.65) (COMPLEXITY . 3)))   
                  (P3007 ((LOSS . 0.80) (COMPLEXITY . 2)))
                  (P3008 ((LOSS . 0.30) (COMPLEXITY . 5))))))
    (assert-equal (non-dominated-sorting scores) '(((P3008) (P3001 P3002 P3005) (P3004 P3003 P3006 P3007))))))
