;;;; Tests for population.lisp

(fiveam:def-suite population-tests)
(fiveam:in-suite population-tests)

(fiveam:test population-initialization
  "Tests the initialization of a population."
  (let* ((p0 (new-population '()))
	 (i0 (new-individual (lambda (x) (apply #'+ x)) :genes '()))
	 (i1 (new-individual (lambda (x) (apply #'+ x)) :genes '(1)))
	 (p1 (new-population (list i0 i1)))
	 (p2 (new-population-initialized 100 (lambda (x) (apply #'+ x)) :gene-amount 100)))
    (fiveam:is (= (length (population-individuals p0))   0))
    (fiveam:is (= (length (population-individuals p1))   2))
    (fiveam:is (= (length (population-individuals p2)) 100))
    (fiveam:is (= (evaluate-fitness p0) 0))
    (fiveam:is (= (evaluate-fitness p1) (/ 1 2)))
    (fiveam:is (< 0 (evaluate-fitness p2) 10000))))

(fiveam:run! 'population-initialization)
