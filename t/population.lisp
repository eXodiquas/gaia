;;;; Tests for population.lisp

(in-package :gaia)

(fiveam:def-suite population-tests)
(fiveam:in-suite population-tests)

(fiveam:test population-initialization
  "Tests the initialization of a population."
  (let* ((p0 (new-population '()))
	 (i0 (new-individual (defit #'+) :genes '()))
	 (i1 (new-individual (defit #'+) :genes '(1)))
	 (p1 (new-population (list i0 i1)))
	 (p2 (new-population-initialized 100 (lambda (x) (apply #'+ x)) :gene-amount 100)))
    (fiveam:is (= (length (population-individuals p0))   0))
    (fiveam:is (= (length (population-individuals p1))   2))
    (fiveam:is (= (length (population-individuals p2)) 100))
    (fiveam:is (= (evaluate-fitness p0) 0))
    (fiveam:is (= (evaluate-fitness p1) (/ 1 2)))
    (fiveam:is (< 0 (evaluate-fitness p2) 10000))))

(fiveam:test converging-population
  "Tests the converging behaviour of a population"
  (let ((p (new-population-initialized 100 (defit #'+) :gene-amount 10)))
    (loop for count upto 100 do
      (setf p (next-generation p)))
    (fiveam:is (> (population-fitness p) 9.0))))

(fiveam:test converging-population-x-squared
  "Tests the converging behaviour of a population on f(x) = -x² + 100"
  (let ((p (new-population-initialized 100 (defit (lambda (y) (+ (- (* y y)) 100))) :gene-amount 1)))
    (loop for count upto 100 do
      (setf p (next-generation p)))
    (fiveam:is (> (population-fitness p) 99.0))))

(fiveam:run! 'population-initialization)
(fiveam:run! 'converging-population)
(fiveam:run! 'converging-population-x-squared)
