;; (defstrategy *steady-state*
;;   (tournament k=4
;;     (sort-by fitness)
;;     (select best -> mutate -> offspring)
;;     (replace worst)))

;; (defstrategy *breeder*
;;   (select top 0.30 -> survivors)
;;   (mutate survivors -> offspring)
;;   (union survivors offspring -> next-generation))

;; (defstrategy *nsga-ii*
;;   (select parents by (tournament k=2))
;;   (mutate parents -> offspring)
;;   (combine population offspring -> combined)
;;   (non-dominated-sort combined -> fronts)
;;   (fill fronts until N:
;;      (sort last-front by crowding-distance)
;;      (take remaining -> next-generation)))

;; (defstrategy *map-elites*
;;   (sample archive -> parent)
;;   (mutate parent -> offspring)
;;   (evaluate offspring -> descriptor fitness)
;;   (discretize descriptor -> cell)
;;   (if (empty? archive[cell]) or (fitness > archive[cell].fitness)
;;       (archive[cell] <- offspring)))

