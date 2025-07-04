(defparameter *points* '((A 0.5 3.0) (B 1.0 2.0) (C 1.5 3.9) (D 2.0 1.0) (E 2 1.75) (F 2 2.8) (G 2.75 2.5) (H 2.75 1.5) (I 3.0 0.5)))

(defun plot-pareto-front (points)
  (let ((objectives (loop for (label x y) in points
                          collect (list x y)))
        (labels (loop for (label x y) in points
                      collect (list x y (symbol-name label)))))
    (gnuplot "Pareto Front"
      (:data "objectives" objectives)
      (:data "labels" labels)
      "reset"
      "set terminal pngcairo size 800,600 enhanced font 'Arial,10'"
      "set output 'figures/pareto_front.png'"
      "set xlabel 'Complexity'"
      "set ylabel 'Error'"
      "set grid"
      "set key off"
      ;; Plot both points and labels
      "plot $objectives using 1:2 with points pointtype 7 pointsize 1.5, \\"
      "     $labels using 1:2:3 with labels offset char 1,1 notitle")))
