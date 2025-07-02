(defun plot-residuals (x y pred)
  (let ((num-x-dimensions (length (first x)))
        (num-y-dimensions (length (first y))))
    (loop for x-dim from 0 below num-x-dimensions do
      (loop for y-dim from 0 below num-y-dimensions do
        (let* ((x-col (column x x-dim))
               (y-col (column y y-dim))
               (pred-col (column pred y-dim))
               (obs-name (format nil "OBS~A" (1+ x-dim)))
               (action-name (format nil "ACTION~A" (1+ y-dim)))
               (basename (format nil "RESIDUAL-~A-~A" obs-name action-name))
               (rows (loop for x-i in x-col
                           for y-i in y-col
                           for p-i in pred-col
                           collect (list x-i y-i p-i))))
          (gnuplot basename
            (:data "data" rows)
            "reset"
            "set terminal pngcairo size 800,600 enhanced font 'Arial,10'"
            (format nil "set output 'figures/~A.png'" basename)
            (format nil "set xlabel '~A'" obs-name)
            (format nil "set ylabel '~A'" action-name)
            "set grid"
            "set key outside"
            "set style line 1 lt 1 lw 2 lc rgb 'blue'"
            "set style line 2 lt 1 lw 2 lc rgb 'red'"
            "plot $data using 1:2 with linespoints ls 1 title 'Truth', \\"
            "     $data using 1:3 with linespoints ls 2 title 'Predicted'"))))))
