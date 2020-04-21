; partitions.lisp 	1/26/95		Ralph Freese

;;;(in-package :user)

;;; 	This uses some of the ideas of Chapter 8 of
;;;
;;;	\ref
;;;	\no \refnumber{Weiss1994}
;;;	\by M. A. Weiss
;;;	\book Data Structures and Algorithm Analysis
;;;	\bookinfo Second Edition
;;;	\publ Benjamin Cummings
;;;	\publaddr Redwood City, California
;;;	\yr 1994
;;;	\endref
;;;
;;;	to implement partitions. See also
;;;
;;;	\ref
;;;	\no \refnumber{Tarjan1975}
;;;	\by R. E. Tarjan
;;;	\paper Efficiency of a good but not linear set union algorithm
;;;	\jour J. Assoc. Comput. Mach.
;;;	\vol 22
;;;	\pages 215--225
;;;	\yr 1975
;;;	\endref
;;;
;;;	The canonical form of a pratition on {0, 1, ..., n-1} will
;;;	a vector V[i] = j, where j is the least element in the block of i.
;;;	unless this least element is i, then V[i] is negative the size of
;;;	the block.
;;;	In intermediate forms, each block will be a tree and this will 
;;;	be stored as a vector U[i] = j, where j is the parent of i, unless
;;;	i has no parent, ie., it is a root, in which case U[i] will 
;;;	the - (size of the tree).
;;;
;;;	root finds the root and performs path compression, if compress is t,
;;;	ie., flattens the tree.
;;;	Note root, normalize-partition and join-blocks can modify 
;;;	their input, but partition-join and -meet don't.

(defun root (i part &optional (compress t))
  (let* ((j (aref part i)))
	(if (< j 0) i 
	    (if compress 
		(setf (aref part i) (root j part compress)) 
		(root j part compress)))))

; (defun root-compress (i part &optional (compress t))
;   (let* ((j (aref part i)))
; 	(if (< j 0) i 
; 	    (setf (aref part i) (root j part)))))

(defun normalize-partition (part)
  (dotimes (i (array-dimension part 0) part) 
	   (let* ((j (root i part t))) 
		 (if (< i j) 
		     (setf (aref part i) (aref part j)
			   (aref part j) i)))))

(defun partition-leq (p1 p2 &optional (compress nil))
  (let* ((n (array-dimension p1 0))
	 (i 0) )
	(loop
	 (if (= i n) (return t))
         (let* ((j (aref p1 i)))
	       (if (and
		    (>= j 0)
		    (not (= (root i p2 compress) (root j p2 nil))) )
		   (return nil))
	       (incf i) ))))

;(defun partition-leq (p1 p2 &optional (compress nil))
;  (let* ((n (array-dimension p1 0))
;	 (i 0) )
;	(loop
;	 (if (= i n) (return t))
;	 (unless (= (root (root i p1 compress) p2 compress) 
;		    (root i p2 compress)) 
;		 (return nil))
;	 (incf i))))



(defun join-blocks (i j part &optional (compress t))
  (let* ((root-i (root i part compress)) 
	 (root-j (root j part compress)) )
    (unless (= root-i root-j)
	    (let* ((size-i (aref part root-i))     ; remember these are the 
		   (size-j (aref part root-j)) )   ; negatives of the sizes
		  (if (<= size-i size-j)      
		      (setf (aref part root-j) root-i
			    (aref part root-i) (+ size-i size-j))
		      (setf (aref part root-i) root-j
			    (aref part root-j) (+ size-i size-j)) )))
    part ))

(defun partition-join2 (p1 p2)
  (let* ((n (array-dimension p1 0))
	 (part (copy-seq p2)) )
	(dotimes (i n)
		 (unless (< (aref p1 i) 0)
			 (join-blocks i (aref p1 i) part t)))
	(normalize-partition part) ))

(defun partition-meet2 (p1 p2 &optional (compress nil))
  (let* ((n (array-dimension p1 0))
 	 (part (make-array n :initial-element -1))
	 (ht-root (make-hash-table :size n :test #'equal)))
	(dotimes (i n part)
	  (let* ((pair (cons (root i p1 compress) (root i p2 compress)))
		 (root (gethash pair ht-root)) )
		(if root
		    (setf (aref part root) (- (aref part root) 1)
			  (aref part i) root )
		    (setf
		     (gethash pair ht-root) i
		     (aref part i) -1 ) )))))


;(defun partition-meet2 (p1 p2 &optional (compress nil))
;  (let* ((n (array-dimension p1 0))
; 	 (part (make-array n :initial-element -1))
;	 (ht-root (make-hash-table :size n :test #'equal))
;	 (ht-count (make-hash-table :size n :test #'equal)))
;	(dotimes (i n)
;	  (let* ((pair (cons (root i p1 compress) (root i p2 compress))) )
;		(if (gethash pair ht-root)
;		    (decf (gethash pair ht-count))
;		    (setf
;		     (gethash pair ht-root) i
;		     (gethash pair ht-count) -1 ))))
;	(dotimes (i n part)
;	  (let* ((pair (cons (root i p1 compress) (root i p2 compress))) )
;		(if (= i (gethash pair ht-root))
;		    (setf (aref part i) (gethash pair ht-count))
;		    (setf (aref part i) (gethash pair ht-root)))))))


; (defun partition-meet2 (p1 p2 &optional (compress nil))
;   (let* ((n (array-dimension p1 0))
; 	 (part (make-array n :initial-element -1)) )
; 	(dotimes (i n)
;           (dotimes (j n)
;		   (if (and (= (root i p1 nil) (root j p1 nil))
;		            (= (root i p2 nil) (root j p2 nil)) )
;		       (join-blocks i j part compress))))
;	(normalize-partition part) ))

(defun partition-join (set)
  "The join of a *nonempty* set of partitons"
  (normalize-partition (partition-join-aux (first set) (rest set))) )

(defun partition-join-aux (p set)
  (if (null set)
      p
      (let* ((n  (array-dimension p 0))
	     (p2 (first set))
	     (part (copy-seq p2)) ) 
	    (dotimes (i n) 
		     (unless (< (aref p i) 0) 
			     (join-blocks i (aref p i) part)))
	    (partition-join-aux part (cdr set)))))

(defun part-vec-2-part-block (part )
  "Changes the *normalized* vector representation into block form"
  (let* ((block-vec (make-array (array-dimension part 0) :initial-element nil)))
	(dotimes (i (array-dimension part 0))
		 (if (< (aref part i) 0)
		     (push i (aref block-vec i))
		     (push i (aref block-vec (aref part i)))))
	(sort (delete-if #'(lambda (x) (null (cdr x))) 
		(map 'list #'nreverse block-vec)) #'general-lex-order) ))


;;; assumes elems in an algebra are already sorted
(defun part-2-cong (part alg)
  (let* ((part2 (part-vec-2-part-block part) ))
    (labels ((foo (block)
    	      (unless (null block)
	        (setf (car block) (aref (algebra-elem-vector alg) (car block)))
	        (foo (cdr block)))) )
	    (mapcar #'foo part2)
	    part2 )))
  
