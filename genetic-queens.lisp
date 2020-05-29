(declaim (optimize (speed 3) (space 0) (debug 1))
	 (inline fitness diagonal-p))

(defmacro as-vector (size exprs)
  `(make-array ,size :fill-pointer ,size :initial-contents ,exprs))

(defun diagonal-p (q1-index q2-index queens-array)
  (= (abs (- (aref queens-array q1-index) (aref queens-array q2-index)))
     (abs (- q1-index q2-index))))

(defun fitness (queens-array)
  (let ((attacking 0))
    (loop :for piv-index :below (length queens-array)
	  :do (loop :for rel-index :below (length queens-array)
		    :when (and (/= piv-index rel-index)
			       (or (= (aref queens-array piv-index)
				      (aref queens-array rel-index))
				   (diagonal-p piv-index rel-index queens-array)))
		      :do (incf attacking))
	  :finally (return attacking))))

(defun breed-queens (first-parent second-parent)
  (let ((break-point (random (length first-parent)))
	(size (length first-parent)))
    (as-vector size (loop :for index :below size
			  :if (<= index break-point)
			    :collect (aref first-parent index)
			  :else :collect (aref second-parent index)))))

(defun generate-queens (size)
  (coerce (as-vector size (loop :repeat size :collect (random size)))
          '(vector fixnum)))

(defun genetic-queens (size k &optional (best nil) (second-best nil))
  (let ((queens `(,(or best (generate-queens size))
		  ,(or second-best (generate-queens size))
		  ,@(loop :repeat (- k 2)
			  :collect (generate-queens size)))))
    (let* ((best (as-vector size (loop :for i :below size :collect i)))
	   (second-best best))
      (loop :for current-queens :in queens
	    :if (< (fitness current-queens)
		   (fitness best))
	      :do (setf second-best best
			best current-queens))
      (if (= (fitness best) 0)
	  (values best)
	  (genetic-queens size k best second-best)))))
