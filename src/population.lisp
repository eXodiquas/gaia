;;;; population.lisp holds the definition of the population for a
;;;; genetic algorithm.

(defclass population ()
  ((individuals :accessor population-individuals
		:initarg :individuals
		:type list
		:documentation "List of all individuals in the population.")
   (fitness :accessor population-fitness
	    :initform 0
	    :type real
	    :documentation "The average fitness of all individuals of the population."))
  (:documentation "Represents a population for a genetic algorithm."))

;;; Constructor functions

(defun new-population (individuals)
  "Returns a newly generated population with the given individuals."
  (let ((result (make-instance 'population :individuals ())))
    (loop for i in individuals do
      (push i (population-individuals result)))
    result))

(defun new-population-initialized (amount ff &key (gene-amount 1))
  "Returns a newly generated population with AMOUNT of individuals and 
the fitness function FF."
  (let ((res-pop (new-population ())))
    (loop for i upto (1- amount) do
      (let ((ind (new-individual ff)))
	(initialize-genes-with-normalized ind gene-amount)
	(evaluate-fitness ind)
	(push ind (population-individuals res-pop))))
    (evaluate-fitness res-pop)
    res-pop))

;;; Methods

(defmethod evaluate-fitness ((p population))
  "Evaluates the fitness of the whole population and averages it. Also
 evaluates the fitness of every individual in the population. 
This method caches the average fitness in the FITNESS slot of the 
POPULATION object."
  (if (not (zerop (length (population-individuals p))))
    (let ((result 0))
      (loop for i in (population-individuals p) do
	(incf result (evaluate-fitness i)))
      (setf (population-fitness p) (/ result (length (population-individuals p)))))
    0))
