;;;; individual.lisp holds the definitions for the smallest building block
;;;; of a genetic algorithm, the individual.

(defclass individual ()
  ((genes :accessor individual-genes
          :initarg :genes
          :type list
          :documentation "Genes represents the configuration of a single
individual in a population.")
   (fitness-function :accessor individual-fitness-function
                     :initarg :fitness-function
                     :documentation "A function in the form of x -> y, where x
is a list of genes and y is a real number that represents the fitness.")
   (current-fitness :accessor individual-current-fitness
                    :initform 0
                    :type real
                    :documentation "Represents the current fitness of the
individual. Technically it is a cached value that gets updated after every
fitness evaluation."))
  (:documentation "A single individual with its genes and fitness."))

;;; Constructor functions

(defun new-individual (ff &key (genes ()))
  "Returns a newly generated individual with the given fitness function and
optinal initialized genes. "
  (make-instance 'individual :fitness-function ff :genes genes))

;;; Functions regarding the fitness of a individual.

(defmethod evaluate-fitness ((i individual))
  "Evaluates the fitness of a given gene and caches the result in the object.
The result is also returned."
  (let ((result
          (funcall (individual-fitness-function i) (individual-genes i))))
    (setf (individual-current-fitness i) result)))

;;; Functions to initialize the genes of a individual.

(defmethod initialize-genes-with-function ((i individual)
                                           (fun function)
                                           (amount integer))
  "Destructively overwrites the genes of the given individual and replaces them
with new genes. The length of the genes list is described by the AMOUNT
parameter and the way the genes are created is described by a function FUN
x -> y, where x is the index of the gene and y is the result of the gene."
  (setf (individual-genes i)
        (loop for i upto (1- amount) collect
                                     (funcall fun i))))

(defmethod initialize-genes-with-normalized ((i individual) (amount integer))
  "Destructively overwrites the genes of the given individual and replaces them
with new genes. The length of the genes list is described by the AMOUNT
parameter and every gene is a number in [0.0,1.0]"
  (initialize-genes-with-function i
                                  (lambda (x)
                                    (declare (ignore x))
                                    (random 1.0))
                                  amount))

;;; Crossover functions

(defmethod uniform-crossover ((i0 individual) (i1 individual))
  "Implementation for the uniform crossover. Takes two individuals I0 and I1 and
recombines their genes with a single uniform cut-point. This method returns two new
individuals, the first one is the left side of I0 and the right side of I1, the
second one is the left side of I1 and the right side of I0."
  (let* ((cut-point (random (length (individual-genes i0))))
	 (i0l (subseq (individual-genes i0) 0 cut-point))
	 (i0r (subseq (individual-genes i0) cut-point))
	 (i1l (subseq (individual-genes i1) 0 cut-point))
	 (i1r (subseq (individual-genes i1) cut-point)))
    (values (new-individual (individual-fitness-function i0) :genes (concatenate 'list i0l i1r))
	    (new-individual (individual-fitness-function i1) :genes (concatenate 'list i1l i0r)))))

;;; Mutation functions

(defmethod real-mutate ((i individual))
  "A mutation working on real numbers with a 50% chance to increase a gene by 50% of its value or decreasing it by 50%."
  (let* ((pos (random (length (individual-genes i))))
	 (val (nth pos (individual-genes i))))
    (if (< (random 1.0) 0.5)
	(setf (nth pos (individual-genes i)) (+ val (* val 0.5)))
	(setf (nth pos (individual-genes i)) (- val (* val 0.5))))))
