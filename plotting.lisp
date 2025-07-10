(in-package :bes)

(setf py4cl:*python-command* "~/Repos/bes/.venv/bin/python3")
(py4cl:import-module "matplotlib.pyplot" :as "plt")

(defun plot-residuals (observations actions predictions)
  "Plot residuals between predicted and true actions for each observation-action-prediction triple.

  OBSERVATIONS: A list of lists representing the observed input features.
  ACTIONS: A list of lists representing the true target values (ground truth).
  PREDICTIONS: A list of lists representing the predicted values.

  Each subplot compares predictions to true values for a specific observation-action pairing, drawing vertical grey lines to indicate residuals."
  (loop for i from 0 below (length (car observations)) do
    (loop for j from 0 below (length (car actions)) do
      (let* ((observations-i (column observations i))
             (actions-j (column actions j))
             (predictions-j (column predictions j)))
        (plt:plot observations-i actions-j :label "TRUTH" :color "blue" :marker "+")
        (plt:plot observations-i predictions-j :label "PREDICTION" :color "red" :marker "+")
        (loop for x in observations-i
              for y1 in predictions-j
              for y2 in actions-j do
                (plt:plot (list x x) (list y1 y2)
                          :color "gray" :linestyle ":" :linewidth 1.0))
        (plt:xlabel (format nil "OBS~A" (1+ j)))
        (plt:ylabel (format nil "ACTION~A" (1+ i)))
        (plt:legend)
        (plt:show)))))

(defun plot-pareto-front (pareto-front)
  "Plot a 2D Pareto front with labelled points.

   PARETO-FRONT: A list of triples (LABEL COMPLEXITY ERROR), where LABEL is a symbol
   identifying each solution, and COMPLEXITY and ERROR are numeric values representing objectives.

   Labels are annotated beside each point, and the front is plotted using scatter points."
  (let ((point-labels (column pareto-front 0))
        (x (column pareto-front 1))
        (y (column pareto-front 2)))
    (loop for xi in x
          for yi in y
          for label in point-labels do
            (plt:text xi yi (symbol-name label)
                      :fontsize 10
                      :ha "left"
                      :va "bottom"))
    (plt:scatter x y :color "blue")
    (plt:xlabel "Complexity")
    (plt:ylabel "Error")
    (plt:grid)
    (plt:show)))
