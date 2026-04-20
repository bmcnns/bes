(in-package :bes)

(defun run-search (mode gym-environment-name dataset-name seed)
  "Search the solution space with a tangled program graph."
  (let* ((*random-state* (sb-ext:seed-random-state seed)))
    (format t "Starting search with seed ~A~%." seed)))
