;;; congrlat-alg.lisp 	01/01/94

;;;(in-package :user)

;;; delete component(s) from this

(defun principal-congruences (alg)
  (let* (
	 (firsts (univ alg))
	 (seconds firsts)
	 (elems (univ alg))
	 (elem-ht (algebra-elem-ht alg))
	 (n (card alg))
	 (m (/ (* n (- n 1)) 2))
	 (count 0)
	 (component-number 0)
	 (component-vec (make-array m :element-type 'fixnum 
			 :initial-element -1))  ; -1 for debugging,delete later
	 (components nil)
	 (edges-vec (make-array m ))
	 ;(principals nil)
	 (stack nil)
	 (number-vec (make-array m :element-type 'fixnum :initial-element 0))
	 (lowlink-vec (make-array m :element-type 'fixnum :initial-element 0))
	 ;;(part-ht (make-hash-table :test #'equal))
	 (stack-ht (make-hash-table :test #'equal)) )
	(labels 
	 (
	  (get-index (vertex-nums) 
	    (+ (* (first vertex-nums) n) 
	       (- (/ (* (first vertex-nums) (+ (first vertex-nums) 1)) 2)) 
	       (second vertex-nums) (- (first vertex-nums)) -1))
	  ;; vertex is a pair of distinct elements of alg
	  ;; index is a single integer indexing it.
	  ;; vertex-numbers is the pair of integers; the indices of the vertex
	  (dfs (vertex index vertex-numbers)
            (let* ((ops (algebra-operations alg)) 
		   (arities (algebra-arities alg)) 
		   (edge-ht (make-hash-table :test #'equal))
		   op arity pos u v vector1 vector2 vector3 )
		  (incf count)
		  (setf (aref edges-vec index) edge-ht)
		  (setf (aref number-vec index) count)
		  (setf (aref lowlink-vec index) count)
		  ;; (push vertex stack)
		  (push vertex-numbers stack)
		  ;; (setf (gethash vertex stack-ht) t)
		  (setf (gethash vertex-numbers stack-ht) t)
		  ;;(setf (gethash vertex part-ht) (list vertex))
		  (loop
		   (if (null ops) (return))
		   (setf op (pop ops) arity (pop arities) pos 0)
		   (loop
		    (if (= pos arity) (return))
		    (setf 
		     vector1 (make-list arity :initial-element elems) 
		     vector2 (make-list arity :initial-element (car elems))
		     vector3 (make-list arity :initial-element (car elems)))
		    (loop
		     ;; the first two setf's should be outside this loop
		     (setf (nth pos vector2) (first vertex) 
			   (nth pos vector3) (second vertex)
			   u (apply op vector2)
			   v (apply op vector3) )
		     (unless (equal u v)
			     (let* ((i (gethash u elem-ht))
				    (j (gethash v elem-ht))
				    vertex-numbers2
				    vertex2 index2 ) 
			       (if (> i j) (psetf i j j i u v v u)) 
			       (setf index2 
			        (+ (* i n) (- (/ (* i (+ i 1)) 2)) j (- i) -1)
				vertex2 (list u v)
				vertex-numbers2 (list i j))
			       (setf (gethash index2 edge-ht) t)
			       (if (= (aref number-vec index2) 0)
			         (progn
				   (dfs vertex2 index2 vertex-numbers2) 
				   (setf (aref lowlink-vec index) 
				         (min (aref lowlink-vec index)
					      (aref lowlink-vec index2))) )
				 ; this progn not needed
				 (progn ; was just vertex2	
				   (if (gethash vertex-numbers2 stack-ht)
				     (setf 
				      (aref lowlink-vec index) 
				      (min (aref lowlink-vec index) 
					   (aref number-vec index2)) ))))
			       ;;(setf (gethash vertex part-ht) 
				     ;;(join2-partition 
				      ;;(gethash vertex part-ht)
				      ;;(gethash vertex2 part-ht)) ) 
				   ))
		     (unless (incq-vector3-with-fixed 
			      vector1 vector2 vector3 elems pos)
			     (return) ))
		    (incf pos) ))
		  ;; here.
		  (if (= (aref number-vec index) (aref lowlink-vec index))
		      (let* ((num (aref number-vec index))
			     (component nil)
			     index2
			     ;; (part (gethash vertex part-ht))
			     )
		        (loop 
			 (if (null stack) (return))
		         (if (>= (aref number-vec 
				   (setf index2 (get-index (car stack)))) 
				 num)
			     (progn 
			      ;;(setf (gethash (car stack) part-ht) part)
			      ;;(pushnew part principals :test #'equal)
			      ;;(push (car stack) component)
			      (push (car stack) component)
			      (setf (aref component-vec index2) 
				    component-number)
			      (remhash (pop stack) stack-ht))
			     (return)))
		        (push component components)
		        (incf component-number)
		        ;;(push component components)
			    )))))
	 (dolist (x firsts)
           (pop seconds)
	   (dolist (y seconds)
             (let* ((vertex (list x y))
		    (vertex-nums (list (gethash x elem-ht) (gethash y elem-ht)))
		    (index (get-index vertex-nums)) )
		   (if (= (aref number-vec index) 0) 
		       (dfs vertex index 
			    (list (gethash x elem-ht) (gethash y elem-ht))))))))
	(let*
	 ((graph-vec (make-array component-number :initial-element nil))
	  (zero-part (make-array n :initial-element -1))
	  (part-vec (make-array component-number :initial-element zero-part))
	  (principals-vec (make-array m))
	  partition
	  (i -1) )
	 (dotimes (i m)
           (let* ((comp (aref component-vec i))) 
		 (maphash 
		  #'(lambda (key val) 
                     (declare (ignore val)) 
		     (let* ((comp-num2 (aref component-vec key)) 
			    (heads (aref graph-vec comp)))
			   (unless (or (= comp comp-num2)
				       (member comp-num2 heads))
			    (setf (aref graph-vec comp) 
				  (cons comp-num2 heads))))) 
		  (aref edges-vec i))))
	 (setf components (nreverse components)
	       graph-vec 
	        (nreverse (topological-sort 
		 (map 'list #'(lambda (x) (list (incf i) x)) graph-vec))))
	 (dolist (lower-covers graph-vec)
		 (setf partition (copy-seq zero-part))
		 (unless (null (second lower-covers))
			 (setf partition (partition-join 
			    (mapcar #'(lambda (x) (aref part-vec x)) 
				    (second lower-covers)))))
		 (dolist (pair (nth (first lower-covers) components))
			 (setf partition
			   (normalize-partition 
			    (join-blocks (first pair) (second pair) 
					 (copy-seq partition)))))
		 (setf (aref part-vec (first lower-covers)) partition) )
	 (dotimes (i m) 
		  (setf (aref principals-vec i)
			(aref part-vec (aref component-vec i))))

	 ;(values 
	  principals-vec
	  ;(map 'list #'part-vec-2-part-block part-vec)
		 ;components 
		 ;edges-vec
		 ;component-number component-vec 
		 ;graph-vec )
	 )))

(defun print-principals (principals-vec)
  (let* (
	 (m (array-dimension principals-vec 0)))
	(dotimes (i m) 
		 (format t "~d  ~s~%" i 
			 (part-vec-2-part-block (aref principals-vec i))))))
