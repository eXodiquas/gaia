;;;; Tests for individual.lisp

(in-package :gaia)

(fiveam:def-suite individual-tests)
(fiveam:in-suite individual-tests)

(fiveam:test individual-initialization
  "Tests the initialization of single individuals."
  (let ((i0 (new-individual (lambda (x) (apply #'+ x)) :genes '(1 2 3 4 5)))
	(i1 (new-individual (lambda (x) (apply #'+ x)) :genes '()))
	(i2 (new-individual (lambda (x) (apply #'+ x)) :genes '(1)))
	(i3 (new-individual (lambda (x) (apply #'+ x)))))
    (fiveam:is (eql (evaluate-fitness i0) 15))
    (fiveam:is (eql (evaluate-fitness i1)  0))
    (fiveam:is (eql (evaluate-fitness i2)  1))
    (fiveam:is (eql (evaluate-fitness i3)  0))))

(fiveam:test individual-init-genes
  "Tests the gene initialization for individuals."
  (let ((i0 (new-individual (lambda (x) (apply #'+ x))))
	(i1 (new-individual (lambda (x) (apply #'+ x))))
	(i2 (new-individual (lambda (x) (apply #'+ x))))
	(i3 (new-individual (lambda (x) (apply #'+ x)))))
    (initialize-genes-with-function i0 (lambda (x) x) 10)
    (initialize-genes-with-function i1 (lambda (x) (* x x)) 5)
    (initialize-genes-with-normalized i2 5)
    (initialize-genes-with-normalized i3 0)
    (fiveam:is (equal (individual-genes i0) '(0 1 2 3 4 5 6 7 8 9)))
    (fiveam:is (equal (individual-genes i1) '(0 1 4 9 16)))
    (fiveam:is (and
		(equal (length (individual-genes i2)) 5)
		(every (lambda (x) (< 0.0 x 1.0)) (individual-genes i2))))
    (fiveam:is (equal (individual-genes i3) nil))))

(fiveam:run! 'individual-tests)
