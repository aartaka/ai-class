(defmacro random-restart-wrapper (repetitions eval-func expr)
  "The wrapper macro to call EXPR REPETITIONS times and find the minimal result via EVAL-FUNC"
  `(reduce #'min (mapcar ,eval-func ,`(loop :repeat ,repetitions
					    :collect ,expr))))

(defvar *walk-times* nil
  "A property list of the people, crossing the bridge, and the time of the crossing")

(defmacro get-time (action)
  "A simple macro to wrap an everpresent
and relatively ugly walk-time acquisition for specific ACTION."
  `(getf *walk-times* ,action))

(defun schedule (num-of-iterations)
  "The function to built a hash-table of temperature
for every iteration of the program up to NUM-OF-ITERATIONS.
Uses alpha/sqrt(step-number) - alpha/sqrt(max-step-number)
to get an exponential zero-ending sequence of probabilities."
  (let ((heat (make-hash-table :test #'equal))
	(alpha 0.45))
    (mapcar #'(lambda (num) (setf (gethash num heat)
				  (- (/ alpha (sqrt num))
				     (/ alpha (sqrt num-of-iterations)))))
	 (loop :for i :from 1 :upto num-of-iterations :collect i))
    heat))

(defun temperature-pick (temperature cost-function options)
  "A function that makes a choise, based on the TEMPERATURE passed to it.
If TEMPERATURE is low, then the best (according to COST-FUNC) option from the OPTIONS list is chosen."
  (if (< (random 1) temperature)
      (elt options (random (length options)))
      (reduce #'(lambda (a b) (min (funcall cost-func a)
				   (funcall cost-func b)))
	      options)))

(defun permutations-two (list)
  "Generates all the unique permutations of the passed LIST, but only if elements are not duplicated."
  (loop :for index :below (length list)
	:append (mapcar #'(lambda (elem) (list (elt list index) elem))
			(subseq list (1+ index)))))

(defun minimal-time-function (generated-path)
  "Finds the time for all ACTIONs in some specific GENERATED-PATH to be made."
  (loop :for action :in generated-path
	:if (listp action)
	  :sum (max (get-time (first action))
		    (get-time (second action)))
	    :into sum
	:else :sum (get-time action) :into sum
	:finally (return sum)))

(defun bridge-gen (initial-state)
  "Generates a random sequence of ACTIONs-moves from the INITIAL-STATE.
Simulated annealing is used for random choise between the given paths, based on TEMPERATURE
from the HEAT-schedule. The behavior for some specific phase is defined by the presence of
TORCH on either side of the bridge."
  (loop
    :with right := nil
    :with left := (copy-list initial-state)
    ; The number of iterations is calculated as (n - 1) * 2 - 1
    ; or, to simplify, 2n - 3, where n is the length of INITIAL-STATE.
    :with num-of-iters := (- (* 2 (length left)) 3)
    :with heat := (schedule num-of-iters)
    :with slowest-of-2 := #'(lambda (a b) (max (get-time a) (get-time b)))
    :for iteration :from 1 :upto num-of-iters
    :for torch-left := t :then (not torch-left)
    :for temperature := (gethash iteration heat)
    :for chosen
      := (if torch-left
	     (temperature-pick temperature slowest-of-2 (permutations-two left))
	     (temperature-pick temperature #'(lambda (someone) (get-time someone)) right))
    :for actions := (append (copy-seq actions) `(,chosen))
    :if torch-left :do (move chosen left right)
      :else :do (move chosen right left)
    :finally (return actions)))

(defmacro move (smth source destination)
  "A macro, wrapping the movement of the 1+ SMTH-ings from SOURCE-sequence
to DESTINATION-sequence, ensuring that no duplicates are left anywhere."
  `(progn (mapcar #'(lambda (item) (push item ,destination))
		  (listify ,smth))
	  (mapcar #'(lambda (item) (setf ,source (delete item ,source)))
		  (listify ,smth))))

(defun listify (x)
  "A one-liner to wrap atomic (non-list) values like X 
into lists and to preserve the inner structure of X if it is a list."
  (if (listp x) x (list x)))

(defun table->list (table)
  "Simple function to make hash-tables more readable, turning them into association lists."
  (loop :for key :being :the :hash-keys :of table
	:collect (list key (gethash key table))))

