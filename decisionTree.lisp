;;Name: Kuan Wen Ng
;;Student Number: 5078052
;;Filename: decisionTree.lisp (ass1)
;;Description: ID3 

;;Column count
(defparameter *column* 0)

;;Data
(defparameter *myData* 0)

;;Atribute names
(defparameter *attName* 0)

;;Attribute values
(defparameter *attValue* 0)

;;Attribute count
(defparameter *attCount* 0)

;;Entropy
(defparameter *rootEntropy* 0)

;;Entropy cal
(defparameter *calEntropy* 0)

;;Data sort according to root
(defparameter *dataSorted* 0)

;;Decision tree
(defparameter *myTree* nil)

;;Struct for tree
(defstruct myTree
	attribute
	value
	branch
)

;;Function to create DT
(defun DecisionTree ()
	;;Initialize data to zero
	(setf *column* nil)
	(setf *myData* nil)
	(setf *attName* nil)
	(setf *attValue* nil)
	(setf *attCount* nil)
	(setf *rootEntropy* nil)
	(setf *calEntropy* nil)
	(setf *dataSorted* nil)
	(setf *myTree* nil)

	;;Get column count
	(let ((in (open "data.txt"))
		)
  		(setf *column* (1+ (count #\Space (read-line in))))
		(close in)
	)

	(let ((in (open "data.txt"))
		)

		;;Get attribute names
		(setf *attName* (make-array *column*))
		(loop for index
			below *column*
			do (setf (aref *attName* index) (read in))
		)

		;;Get data
		(setf *myData* (make-array *column*))
		(when in
   			(loop for word = (read in nil)
				for index below (1+ *column*)
         			while word do (cond
							((= index 4)
								(setf index 0)
								(setf (aref *myData* index) (cons word (aref *myData* index)))
							)
							(t
								(setf (aref *myData* index) (cons word (aref *myData* index)))
							)
						)
			)
    			(close in)
		)
	)

	;;Get attribute values
	(setf *attValue* (make-array *column*))

	(loop for index below *column*
		do (setf (aref *attValue* index) (remove-duplicates (aref *myData* index))
		)
	)

	;;Dimensional array for class count
	(setf *attCount* (make-array (1- *column*)))
	(loop for index below (1- *column*)
		do (setf (aref *attCount* index) (make-array (length (aref *attValue* index)))
		)
	)
	;;Terminal class count
	(loop for index below (1- *column*)
		do (loop for index1 below (length (aref *attCount* index))
			do (setf (aref (aref *attCount* index) index1) (make-array (length (aref *attValue* (1- *column*))))
			)
		)
	)
		
	;;Get attribute count
	(loop for index below (1- *column*)
		do (loop for index1 below (length (aref *attValue* index))
			do (loop for indexAll below (length (aref *myData* index))
				do (if (string-equal (nth indexAll (aref *myData* index)) (nth index1 (aref *attValue* index)))
					(loop for index2 below (length (aref *attValue* (1- *column*)))
						do (if (string-equal (nth indexAll (aref *myData* (1- *column*))) (nth index2 (aref *attValue* (1- *column*))))
							(setf (aref (aref (aref *attCount* index) index1) index2)
							(cons (nth index2 (aref *attValue* (1- *column*))) (aref (aref (aref *attCount* index) index1) index2))
							)
						)
					)
				)
			)
		)
	)

	(loop for index below (1- *column*)
		do (loop for index1 below (length (aref *attCount* index))
			do (loop for index2 below (length (aref *attValue* (1- *column*)))
				do (setf (aref (aref (aref *attCount* index) index1) index2)
				(count (nth index2 (aref *attValue* (1- *column*))) (aref (aref (aref *attCount* index) index1) index2))
				)
			)
		)
	)

	;;Unusable block of code to visualize data
	(setf *rootEntropy* (make-array (1- *column*)))
	(loop for index below (1- *column*)
		do (setf (aref *rootEntropy* index) (make-array (length (aref *attvalue* (1- *column*))) :initial-element 0)
		)
	)
	(loop for index below (1- *column*)
		do (loop for index1 below (length (aref *attCount* index))
			do (loop for index2 below (length (aref *attValue* (1- *column*)))
				do (setf (aref (aref *rootEntropy* index) index2)
				(+ (aref (aref *rootEntropy* index) index2) (aref (aref (aref *attCount* index) index1) index2))
				)
			)
		)
	)

	;;Calculate entropy
	(setf *calEntropy* (make-array (1- *column*) :initial-element 0))
	(loop for index below (1- *column*)
		do (loop for index1 below (length (aref *attCount* index))
			do (setf (aref *calEntropy* index) (+ (calEntropy (aref (aref *attCount* index) index1)) (aref *calEntropy* index))
			)
		)
	)

	;;Sort attributes in order on entropy
	(sortEntropy)

	;;Create tree
	(createTree)
	(return-from DecisionTree *myTree*)

)

;;Function to calculate information gain
(defun calEntropy (node)
	(let ((tempEntropy 0)
		(tempCount 0)
		)

		(loop for index below (length node)
			do (setf tempCount (+ tempCount (aref node index))
			)
		)

		(loop for index below (length node)
			do (if (> (aref node index) 0)
				(setf tempEntropy (+ (- (* (/ (aref node index) tempCount ) (log (/ (aref node index) tempCount) 2))) tempEntropy)
				)
			)
		)
		(setf tempEntropy (* (/ tempCount (length (aref *mydata* 0))) tempEntropy))

		(return-from calEntropy tempEntropy)
	)
)

;;Bubble sort to sort attribute by entropy
(defun sortEntropy ()
	(let ((tempData 0)
		(tempAttName 0)
		(tempAttValue 0)
		(tempAttCount 0)
		(tempCalEntrophy 0)
		(sorted nil)
		)

		(loop with sorted = nil until sorted do
			(setf sorted t)
			
			(loop for index below (- *column* 2)
				do (cond
					((> (aref *calEntropy* index) (aref *calEntropy* (1+ index)))
						(setf tempData (aref *myData* index))
						(setf tempAttName (aref *attName* index))
						(setf tempAttValue (aref *attValue* index))
						(setf tempAttCount (aref *attCount* index))
						(setf tempCalEntrophy (aref *calEntropy* index))

						(setf (aref *myData* index) (aref *myData* (1+ index)))
						(setf (aref *attName* index) (aref *attName* (1+ index)))
						(setf (aref *attValue* index) (aref *attValue* (1+ index)))
						(setf (aref *attCount* index) (aref *attCount* (1+ index)))
						(setf (aref *calEntropy* index) (aref *calEntropy* (1+ index)))

						(setf (aref *myData* (1+ index)) tempData)
						(setf (aref *attName* (1+ index)) tempAttName)
						(setf (aref *attValue* (1+ index)) tempAttValue)
						(setf (aref *attCount* (1+ index)) tempAttCount)
						(setf (aref *calEntropy* (1+ index)) tempCalEntrophy)

						(setf sorted nil)
					)

					(t
					)
				)
			)
		)
	)
)

(defun setBranch (node tempAtt tempVal)
	(let ((exist nil)
		(tempBranch nil)
		)
		(loop for index below (length (myTree-branch node)) do
			(cond
				((eq (myTree-value (elt (myTree-branch node) index)) (car tempVal))
					(setf tempAtt (cdr tempAtt))
					(setf tempVal (cdr tempVal))
					(setf exist t)

					(if (not (eq tempAtt nil))
						(setBranch (elt (myTree-branch node) index) tempAtt tempVal)
					)
				)
				(t
				)
			)
		)
		(cond
			((eq exist nil)
				(setf tempBranch (make-myTree :attribute (car tempAtt)
									:value (car tempVal)
									:branch (make-array 0 :fill-pointer 0 :adjustable t)))
				(vector-push-extend tempbranch (myTree-branch node))
				(setf tempAtt (cdr tempAtt))
				(setf tempVal (cdr tempVal))

				(if (not (eq tempAtt nil))
					(setBranch (elt (myTree-branch node) (1- (length (myTree-branch node)))) tempAtt tempVal)
				)				
			)
			(t
			)
		)
	)
)

;;Could not figure out to recursively build nested list
(defun createTree ()
	(let ((tempAtt nil)
		(tempVal nil)
		)
		(setf *myTree* (make-myTree :attribute 'ROOT
						:value 'ROOT
						:branch (make-array 0 :fill-pointer 0 :adjustable t)))

		(loop for indexAll below (length (aref *myData* 0)) do
			(setf tempAtt nil)
			(setf tempVal nil)
			(loop for indexColumn below *column* do
				(setf tempAtt (cons (aref *attName* indexColumn) tempAtt))
				(setf tempVal (cons (nth indexAll (aref *myData* indexColumn)) tempVal))
			)
			(setf tempAtt (reverse tempAtt))
			(setf tempVal (reverse tempVal))
			(setBranch *myTree* tempAtt tempVal)
		)
	)
)
