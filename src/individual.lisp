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
