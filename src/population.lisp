;;;; population.lisp holds the definition of the population for a
;;;; genetic algorithm.

(defclass population ()
  ((individuals :accessor population-individuals
		:initarg :individuals
		:type list
		:documentation "List of all individuals in the population.")
   (individual-type :accessor population-individual-type
		    :initarg :individual-type
		    :initform :real
		    :type symbol
		    :documentation "Type of the genes of the individuals in the
population. This is used to determine the correct algorithms for recombination and
mutation. Currently there are the following supported types: :real")
   (fitness :accessor population-fitness
	    :initform 0
	    :type real
	    :documentation "The average fitness of all individuals of the population.")
   (crossover :accessor population-crossover
	      :initarg :crossover
	      :initform :uniform
	      :type symbol
	      :documentation "The type of crossover that should be performed in each generation.
Currently there are the following implementations: :uniform")
   (mutation-rate :accessor population-mutation-rate
		  :initarg :mutation-rate
		  :initform 0.05
		  :type real
		  :documentation "The chance of a gene to randomly mutate. The mutation rate has to be in the
intervall [0.0, 1.0]."))
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

(defmethod fitness-biased-selection ((p population) (n integer))
  "Takes a population P and returns the N fittest individuals in a list."
  (loop for i in (sort
		  (population-individuals p)
		  #'(lambda (x y) (> (individual-current-fitness x) (individual-current-fitness y))))
	for j from 0 upto (1- n) collect i))

(defmethod uniform-recombination ((p population))
  "Applies uniform crossover on 50% of the fittest individuals and adds the children to the next
generation. This is repeated until the next generation has exactly as many children as the original
population."
  (let* ((pop-size (length (population-individuals p)))
	 (fittests (fitness-biased-selection p (ceiling pop-size 2)))
	 (next-gen (new-population '())))
    (loop for i upto (ceiling pop-size 2) do
      (let ((p0 (nth (random (length fittests)) fittests))
	    (p1 (nth (random (length fittests)) fittests)))
	(multiple-value-bind (c0 c1) (uniform-crossover p0 p1)
	  (when (< (length (population-individuals next-gen)) pop-size)
	    (push c0 (population-individuals next-gen)))
	  (when (< (length (population-individuals next-gen)) pop-size)
	    (push c1 (population-individuals next-gen))))))
    (evaluate-fitness next-gen)
    next-gen))

(defmethod recombination ((p population))
  "Applies the configured recombination algorithm."
  (case (population-crossover p) (:uniform (uniform-recombination p))))

(defmethod mutate-population ((p population))
  "Applies the configured mutation algorithm."
  (loop for i in (population-individuals p) do
    (when (< (population-mutation-rate p) (random 1.0))
      (case (population-individual-type p) (:real (real-mutate i))))))

(defmethod next-generation ((p population))
  "Takes a population P and applies the configurated steps to calculate
the next population P' which is returned."
  (let ((recombinated-population (recombination p)))
    (mutate-population p)
    (evaluate-fitness recombinated-population)
    recombinated-population))
