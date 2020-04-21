;;; congrlat-lat.lisp 	02/27/93
;;;		revised 03/06/93

;;; This screws up on the one element lattice. Fix it.

;;; It might be better to use a trie so that the join and meet
;;; actually return an element in (lattice-elems (con lat)),
;;; especially since congruences can contain info about the quotient, etc.
;;; I am changing the definition of (lattice-lower-covers (con lat))
;;; to first find the element in (univ (con lat)), then lookup
;;; up in the hash table, since otherwise it fails.

;;;(in-package :user)

(defmacro make-zero-bitvector (size)
  `(make-array ,size :element-type 'bit :initial-element 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	New congruence representation for finite lattices
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This uses our fast algorithms from the monograph (using G(L)) to
;;; calculate the equivalence classes of ji's (a ~ b if Cg(a,a_*) = 
;;; Cg(b,b_*) ). A Congruence theta is then represented as those blocks 
;;; such that Cg(a,a_*) \leq theta. We define a structure, congruence, 
;;; to represent congruences. Internally, a bit vector is used to 
;;; indicate if a block is collapsed. The quotient lattice, L/theta, 
;;; is ismorphic to the join closure in L of the set A of those 
;;; ji's not collapsed. The correxponding homomorphism sends 
;;; x |-> \Join {a \in A : a \leq x}. Also <x,y> \in theta if and only
;;; if for all a \in J(L) with a \leq x, a \nleq y (or visa versa),
;;; a is collapsed by theta.


;;; The next function calculates the strong connected components of
;;; a directed graph. It uses Tarjan's depth first search algorithm,
;;; see Aho's book referenced below.

;;; scc stands for strong connect components of a directed graph. 
;;; (adj-fn x) returns the nodes y such that there is an edge x -> y. 
;;; (rev-adj-fn x) returns the nodes y such that there is an edge y -> x.
;;; This function returns 3 values: a partition of elements into 
;;; scc's, an integer valued hash table mapping each element to its
;;; index in the partition, and the number of blocks of the partition.
;;; It uses the two pass version of Tarjan's algorithm presented in 

;;;	\ref
;;;	\no \refnumber{AhoHopcroftUllman1983}
;;;	\by A. Aho, J. Hopcroft, and J. Ullman
;;;	\book Data Structures and Algorithms
;;;	\yr 1983
;;;	\publ Addison Wesley
;;;	\publaddr Reading, Mass.
;;;	\endref

;;; scc-1 is the first pass; scc-2 is the second.

(defun scc (elems adj-fn rev-adj-fn)
  (let* ((c 0)
	 (n (length elems))
	 (vec (scc-1 elems adj-fn n))  ; scc-1 does the first pass of Aho's
	 (sccs nil)		       ; two pass depth first search.
	 (current-scc nil)
	 (scc-ht (make-hash-table-safe :size n :test #'equal))
	 (visited (make-hash-table-safe :size n :test #'equal)) )
	(labels ((scc-2 (x)
		   (setf (gethash x visited) t) 
		   (setf (gethash  x scc-ht) c)
		   (push x current-scc)
		   (dolist (y (funcall  rev-adj-fn x))
			   (unless (gethash y visited)
				   (scc-2 y)) ) )) 
		(dotimes (i n) 
			 (let ((x (svref vec (- n i 1))))
			      (unless (gethash x visited) 
				      (scc-2 (svref vec (- n i 1))) 
				      (push current-scc sccs) 
				      (setf current-scc nil) 
				      (incf c)) ))
		(values (nreverse sccs) scc-ht c))))

(defun scc-1 (elems adj-fn n)
  (let* (
	 (count -1)
	 (vec (make-array n))
	 ;; (num-hash (make-hash-table-safe :size n :test #'equal))
	 (visited (make-hash-table-safe :size n :test #'equal)) ) 
	(labels ((dfs-aux (x) 
		   (setf (gethash x visited) t) 
		   (dolist (y (funcall  adj-fn x))
			   (unless (gethash y visited)
				   (dfs-aux y)) )
                   (setf (svref vec (incf count)) x) ))
		(loop 
		 (if (null elems) (return vec)) 
		 (unless (gethash (car elems) visited) 
			 (dfs-aux (car elems)))
		 (pop elems) ) ) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;     	New Congruence Lattice Representation
;;;;    New version. Put this in congrlat.lisp. 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; should be lattice-congruence

(defstruct (congruence 
	       (:constructor create-congruence)
	       (:print-function
		(lambda (congr stream depth)
		  (declare (ignore depth))
		  (cond 
		   (*print-pretty* 
		    (format stream 
			    "#<congruence, ji's, a, collapsed to a_*: { ")
		    (dolist (a (jis-collapsed congr))
			    (format stream "~s " a)) 
		    (format stream "}>") )
		   (t
		    (format stream "#<congruence ")
		    (dotimes
		     (i (array-dimension (congruence-vector congr) 0))
		     (format stream "~d" (bit (congruence-vector congr) i)))
		    (format stream ">")
			)))))
  vector	; a bit vector.  vector[i] = t if the ith 
		; strong cc of G(L) is below the congr.
  ;; ji-ht		; ji-ht[a] = t if Cg(a,a_*) \leq the congr
  ;; mi-ht	
  lat		; the lattice
  ;; congr-lat	; the congruence lattice
  quotient-lat	; lat/congr
  std-homo	; the standard homomorphism lat ->> quotient-lat
  )


;;; We have to decide if we should keep both the join and meet irreds.
;;; Assuming we just keep the join irreducibles,
;;; We need to transform sccs to have only the join irreds, make a 
;;; new scc-ht (the old one is probably not needed), make a vector
;;; translating the indices back to the blocks. The we need to 
;;; make a lookup table of the induced relation from the graph 
;;; (this will have to be passes). Then we need to use our make-ideals
;;; to make the ideals of each element. The congruence lattice need to
;;; deal with the order ideals.


(defun congrlat-aux (lat)
  (let* (
	 (ji-list (lattice-ji-list lat))
	 (conlat (lattice-con lat))
	 ;; make the ji's and mi's disjoint.
	 (jis-2 (mapcar #'(lambda (x) (cons x 0)) (lattice-ji-list lat)))
	 (mis-2 (mapcar #'(lambda (x) (cons x 1)) (lattice-mi-list lat)))
	 (elems (append jis-2 mis-2))
	 (r (length elems))
	 (adj-ht (make-hash-table-safe :size r))
	 (rev-adj-ht (make-hash-table-safe :size r)) )
	(labels (
		 (l-star (x) (first (funcall (lattice-lower-covers lat) x)))
		 (u-star (x) (first (funcall (lattice-upper-covers lat) x))) )
		(dolist (a jis-2)
		 (dolist (q mis-2)
			 (when (and (lssql (car a) (u-star (car q)) lat)
				    (not (lssql (car a) (car q) lat)))
			       (setf (gethash a adj-ht) 
				     (cons q (gethash a adj-ht)))
			       (setf (gethash q rev-adj-ht) 
				     (cons a (gethash q rev-adj-ht))) )))
		(dolist (a jis-2)
		 (dolist (q mis-2)
			 (when (and (lssql (l-star (car a)) (car q) lat)
				    (not (lssql (car a) (car q) lat)))
			       (setf (gethash q adj-ht) 
				     (cons a (gethash q adj-ht)))
			       (setf (gethash a rev-adj-ht) 
				     (cons q (gethash a rev-adj-ht))) )))
		(multiple-value-bind 
		 (scc scc-ht count)
		 (scc elems 
		      #'(lambda (x) (gethash x adj-ht))
		      #'(lambda (x) (gethash x rev-adj-ht)))
		 ;; (declare (ignore scc))
		 ;; above-array [i,j] will be t if i -> j in the 
		 ;; induced order on the indices.
		 ;; aboves is a list of elements of the form (a (b c ... ))
		 ;; where a -> b, etc.
		 (let ((above-array (make-array (list count count)))
		       (scc-vec (make-array count))
		       ideals
		       (k 0)
		       ;; scc-ht2 will be for the ji's only.
		       (scc-ht2 (make-hash-table-safe :size (length ji-list)
						 :test #'equal))
		       (aboves nil))
		      (dotimes (i count) (push (list i nil) aboves))
		      (setf aboves (nreverse aboves))

		      (dolist (x elems)
		       (dolist (y (gethash x adj-ht)) 
			(unless (= (gethash x scc-ht) 
				   (gethash y scc-ht)) 
				(setf (aref above-array 
					    (gethash x scc-ht) 
					    (gethash y scc-ht)) t))))
		      
		      (let* ((aboves2 aboves)
			     (above (pop aboves2)) )
		            (dotimes (i count)
		             (dotimes (j count)
			      (if (aref above-array i j)
			        (setf (second above) (cons j (second above)))))
                             (setf above (pop aboves2))))
		      (setf ideals (make-ideals
				    (make-filters 
				     (topological-sort aboves)))) 
		      ;; transfer the info from scc-ht to scc-ht2:
		      (dolist (x elems)
			      (if (= (cdr x) 0)
				  (setf (gethash (car x) scc-ht2)
					(gethash x scc-ht))))
		      ;; conlat's ht will map J(L) to the scc's indeces
		      (setf (lattice-hash-table conlat) scc-ht2)
		      ;; construct scc-vec and put it in symbol-names slot.
		      (dolist (x scc)		; x is a scc
			      (dolist (y x)	; y = (a . 0) or (q . 1)
				      (if (= (cdr y) 0) 
					  (setf 
					   (svref scc-vec k) 
					   (push (car y) (svref scc-vec k)))))
			      (incf k))
		      (setf (lattice-symbol-names conlat) scc-vec)
		      (setf (lattice-ji-list conlat) 
			    (mapcar #'(lambda (x) 
				       (make-congruence (second x) count lat)) 
				    ideals ))
		      )))))

(defun make-congruence (ideal size lat)
  (let* ((congr (create-congruence))
	 (vec (make-zero-bitvector size)) )
	(setf (congruence-lat congr) lat)
	(dolist (j ideal) 
		(setf (bit vec j) 1))
	(setf (congruence-vector congr) vec)
	congr))

;;; Equality test for congruences. (equal works on bit vectors.)

(defun equal-congruence (cong1 cong2)
  (and
   (eq (congruence-lat cong1) (congruence-lat cong2))
   (equal (congruence-vector cong1) (congruence-vector cong2))))

;; scc-ht	; scc-ht[a] = i if Cg(a,a_*) is in the ith scc of 

;;; 7/14/94 added a few lines at the beginning to cover semilattices.
;;;         they pass the lattice-p test, so I am catching it here.
;;;         The code is not indented properly.

(defun con-lat (lat elems-p)
 (if (eql (lattice-type lat) 'finite-semilattice)
  (if (lattice-con lat) 
    (lattice-con lat) 
    (make-congrlat lat))
    ;; else to first if:
 (macrolet ((vec2congr (vec lat)
                `(let* ((con (create-congruence)))
		       (setf (congruence-lat con) ,lat)
		       (setf (congruence-vector con) ,vec)
		       con )))
  (unless (lattice-con lat)
   (let ((temp (create-lattice))
	 count
	 hold-vec )

    (setf (lattice-con lat) temp)
    ;; this sets ji-list, hash-table and symbol-names of temp
    (congrlat-aux lat)		
    (setf count (length (lattice-ji-list temp))
	  hold-vec (make-zero-bitvector count))
    (setf (lattice-type temp) 'congruence)
    ;; add a result vec to hold the bit-and below.
    (setf (lattice-lssql temp) 
	  #'(lambda (x y)
	     (equal (congruence-vector x) 
		    (bit-and (congruence-vector x) 
			     (congruence-vector y)
			     hold-vec ))))
    (setf (lattice-grtrql temp) 
	  #'(lambda (x y)
	     (equal (congruence-vector y) 
		    (bit-and (congruence-vector x) 
			     (congruence-vector y)
			     hold-vec ))))
    (setf (lattice-zero temp) 
	  (vec2congr (make-zero-bitvector count) lat))
    (setf (lattice-one temp)
	  (vec2congr (bit-not (make-zero-bitvector count)) lat))

    (setf (lattice-join temp) 
	  #'(lambda (args) 
	     (vec2congr 
	      (reduce #'bit-ior
		      (mapcar #'congruence-vector args) 
		      :initial-value (congruence-vector (lattice-zero temp))) 
	      lat)))
    (setf (lattice-meet temp)
	  #'(lambda (args) 
             (vec2congr 
	      (reduce #'bit-and
		      (mapcar #'congruence-vector args) 
		      :initial-value (congruence-vector (lattice-one temp))) 
	      lat))) ))
  (when (and elems-p (not (lattice-elem-list (lattice-con lat))))
     (let* (
	    (jis (lattice-ji-list (con lat)))
	    (count (length jis))
	    (bit-zero (make-zero-bitvector count))
	    (elements (list bit-zero)) 
	    ;; bit-vec is used to more efficiently hold the
	    ;; temporary joins of bit vectors.
	    (bit-vec (make-zero-bitvector count)) )
       ;; dist-closure(nil, set, zero) is the join closure of a set 
       ;; of join irreducibles in a 
       ;; distributive lattice. In this version the elements of the 
       ;; lattice are bit vectors, but the algorithm would work for
       ;; any distributive lattice.
       ;; Suppose set is (x_0, ... , x_{k-1}), then lst1 will be an
       ;; antichain of the form (x_{i_0}, ..., x_{i_{s-1}}) with
       ;; i_0 > ... > i_{s-1}, lst2 is some tail of set, disjoint from 
       ;; lst1. join-lst1 is the join of lst1. The answer is pushed
       ;; onto elements.
       ;; It this latest version, bv-anti is the join of an antichain,
       ;; bv-x is a single element, everything in the antichain 
       ;; incomparable with everything in lst2 and also with bv-x.
       ;; Note that jis is assumed to be topologically sorted so
       ;; that bv-x is comparable with something in lst2 if and only if
       ;; it is below something in lst2
       ;; 
       (labels 
	((remove-comps (x lst) 
	   (if (null lst)
	       nil
	       ;; This assume lst is topologicall sorted.
	       (if (equal (bit-and x (car lst) bit-vec) x) 
		   (remove-comps x (cdr lst))
		   (cons (car lst) (remove-comps x (cdr lst))))))
	 (close-aux (bv-anti bv-x lst2)
	   (let* (			
		  ;;(lst3 nil)
		  (bv-anti2 (bit-ior bv-anti bv-x)))
		 (push bv-anti2 elements)
		 (when lst2
		       (close-aux bv-anti (car lst2) (cdr lst2))
		       (setf lst2 (remove-comps bv-x lst2))
		       (if lst2
			   (close-aux bv-anti2 (car lst2) (cdr lst2)))))))
	;; This sets elements. It assumes jis is topologically sorted.
	(when jis 
	      (close-aux bit-zero (congruence-vector (car jis)) 
			 (mapcar #'congruence-vector (cdr jis))))
	(let*   ((temp (lattice-con lat)) 
	         (elems (mapcar #'(lambda (x) (vec2congr x lat))
				elements))
		 (num-elems (length elems))
		 (upper-covers nil)
		 (filters nil)
		 (fn (lattice-grtrql temp))
		 (ht (make-hash-table-safe 
		      :size (floor (* *ht-factor* num-elems)) 
		      :test #'equal))
		 (ht-uc (make-hash-table-safe 
			 :size (floor (* *ht-factor* num-elems)) 
			 :test #'equal))
		 (ht-non-cov-p (make-hash-table-safe 
				:size (floor (* *ht-factor* num-elems)) 
				:test #'equal)))

		;; next part is taken from make-finite-lattice-from-grtrql
		(dolist (x elems) 
		 (dolist (y elems) 
			 (if (funcall fn y x) 
			     (setf (gethash x ht) (cons y (gethash x ht))))))
		(setq filters (topological-sort
		       (mapcar #'(lambda (x) (list x (gethash x ht))) elems)))
	        (setf (lattice-elem-list temp) (mapcar #'first filters))
		(setq elems (reverse (lattice-elem-list temp)))
		(dolist (x elems) 
			(clrhash ht-non-cov-p)
			(dolist (y (gethash x ht))
				(if (not (equal y x)) 
				    (dolist (z (gethash y ht-uc))
				      (setf (gethash z ht-non-cov-p) t))))
			(dolist (y (gethash x ht))
				(if (not (equal y x)) 
				    (unless (gethash y ht-non-cov-p)
				      (setf (gethash x ht-uc)
					    (cons y (gethash x ht-uc)))))))
		(dolist (x elems)
			(push (list x (gethash x ht-uc)) upper-covers))
		;; the upper-covers is not topologically sorted
		(setf (lattice-upper-covers-list temp) upper-covers)
		(setf (lattice-upper-covers temp) 
		      #'(lambda (x) 
		         (gethash (find x elems :test #'equal-congruence) 
				  ht-uc)) )
		(setf (lattice-filters temp) filters)
		(setf (lattice-ideals temp) (make-ideals filters))
		(multiple-value-bind
		 (lc ht-lc) (make-ideals upper-covers)
		 (setf (lattice-lower-covers-list temp) lc)
		 (setf (lattice-lower-covers temp)
		       #'(lambda (x) 
	                  (gethash (find x elems :test #'equal-congruence)
				   ht-lc)) ) )
		(setf (lattice-mi-list temp)
		      (remove-if 
		       #'(lambda (x) 
                          (let* ((uc (funcall (lattice-upper-covers temp) x)))
				(or (null uc) (cdr uc))))
		       elems) )
		))))
    (lattice-con lat) )))

;;; This represents lat/congr as a join subsemilattice of lat by taking
;;; the join closure of the ji elements not collapsed to their lower 
;;; cover. It also sets the standard homomorphism. It could be more
;;; efficient because we already know its join irreducibles and its 
;;; join function.

(defun quotient (lat congr)
 (if (congruence-quotient-lat congr) 
  (congruence-quotient-lat congr) 
  (let* ((con (con-lat lat t))	; change to con eventually
	 (j (length (lattice-ji-list con)))
	 (vec (congruence-vector congr))
	 lat2
	 (set nil) )
	(dotimes (i j)
	 (if (= (bit vec i) 0)
	     (setf set (append (svref (lattice-symbol-names con) i) set))))
	(setf lat2
	      (make-finite-lattice-from-grtrql 
		    (cons (lattice-zero lat) 
			  (closure set #'(lambda (&rest args)
					  (funcall (lattice-join lat) args))))
		    (lattice-grtrql lat)))
	(setf (congruence-quotient-lat congr) lat2)
	(setf (congruence-std-homo congr)
	      #'(lambda (x)
		  (unless (member x (lattice-elem-list lat) :test #'equal)
			  (error "~s is not in the domain." x))
		  (funcall (lattice-join lat2)
			   (remove-if-not 
			    #'(lambda (y)
				      (funcall (lattice-lssql lat) y x))
			    (lattice-ji-list lat2)))))
	lat2 )))

(defun congruent-p (x y congr)
  (let* ((con-vec (congruence-vector congr))
	 (lat (congruence-lat congr))
	 (jis (lattice-ji-list lat))
	 (scc-ht (lattice-hash-table (con lat))) )
	(loop
	 (if (null jis)
	     (return t)
	     (let* ((a (pop jis)))
		   (if (and 
			;; Next line says Cg(a,a_*) \nleq congr
			(= 0 (bit con-vec (gethash a scc-ht)))
			(or
			 (and (lssql a x lat) (not (lssql a y lat)))
			 (and (lssql a y lat) (not (lssql a x lat)))))
		       (return-from congruent-p nil)))))))


(defun jis-collapsed (th)
  (let* ((lat (congruence-lat th))
	 (ht (lattice-hash-table (con lat)))
	 (bit-vec (congruence-vector th))
	 (ans nil)
	 (jis (lattice-ji-list lat)) )
	(dolist (a jis ans)
		(if (= (bit bit-vec (gethash a ht)) 1)
		    (push a ans)))))

;;; The next functions test subidrect irreduciblity and simplicity 
;;; of a lattice, using the O(n^2) algorithm of the monograph. If
;;; Con L has already been calcualted, this is used to determine the
;;; answer, but it can be called without Con L first having been computed.

(defun si-p (&optional (lat *lat*))
 (if (lattice-con lat)
  (let* ((con (con lat))
	 (jis (lattice-ji-list con))
	 (leq (lattice-lssql con))
	 (a (first jis)) ) 
	(loop
	 (if (null jis)
	     (return t)
	     (unless (funcall leq a (pop jis))
		     (return nil)))))
  (let* (
	 ;; make the ji's and mi's disjoint.
	 (jis-2 (mapcar #'(lambda (x) (cons x 0)) (lattice-ji-list lat)))
	 (mis-2 (mapcar #'(lambda (x) (cons x 1)) (lattice-mi-list lat)))
	 (elems (append jis-2 mis-2))
	 (r (length elems)) 
	 (adj-ht (make-hash-table-safe :size r))
	 (rev-adj-ht (make-hash-table-safe :size r)) )
	(labels (
		 (l-star (x) (first (funcall (lattice-lower-covers lat) x)))
		 (u-star (x) (first (funcall (lattice-upper-covers lat) x))) )
		(dolist (a jis-2)
		 (dolist (q mis-2)
			 (when (and (lssql (car a) (u-star (car q)) lat)
				    (not (lssql (car a) (car q) lat)))
			       (setf (gethash a adj-ht) 
				     (cons q (gethash a adj-ht)))
			       (setf (gethash q rev-adj-ht) 
				     (cons a (gethash q rev-adj-ht))) )))
		(dolist (a jis-2)
		 (dolist (q mis-2)
			 (when (and (lssql (l-star (car a)) (car q) lat)
				    (not (lssql (car a) (car q) lat)))
			       (setf (gethash q adj-ht) 
				     (cons a (gethash q adj-ht)))
			       (setf (gethash a rev-adj-ht) 
				     (cons q (gethash a rev-adj-ht))) )))
		(multiple-value-bind 
		 (scc scc-ht count)
		 (scc elems 
		      #'(lambda (x) (gethash x adj-ht))
		      #'(lambda (x) (gethash x rev-adj-ht)))
		 (declare (ignore scc))
		 ;; vec is boolean valued, defined on the indices of the
		 ;; eq classes: t if it is not minimal
		 (let ((vec (make-array count))
		       (min 0))
		      (dolist (x elems)
			      (dolist (y (gethash x adj-ht))
				      (unless (eql (gethash x scc-ht)
						   (gethash y scc-ht))
					      (setf (svref vec
							   (gethash y scc-ht))
						    t))))
		      (dotimes (i count)
			       (unless (svref vec i) (incf min)))
		      (values 
		       (= min 1)
		       (if (= count 1) 
			   (format nil "In fact, this lattice is simple.")
			   (format nil 
			    "There are ~d join irreducible congruences, ~
			       ~d minimal." 
			       count 
			       min)))))))))
      

(defun simple-p (&optional (lat *lat*))
 (if (lattice-con lat)
  (if (and (cdr (univ lat)) (not (cdr (lattice-ji-list (con lat))))) t nil)
  (let* (
	 (jis-2 (mapcar #'(lambda (x) (cons x 0)) (lattice-ji-list lat)))
	 (mis-2 (mapcar #'(lambda (x) (cons x 1)) (lattice-mi-list lat)))
	 (elems (append jis-2 mis-2))
	 (r (length elems)) 
	 (adj-ht (make-hash-table-safe :size r))
	 (rev-adj-ht (make-hash-table-safe :size r)) )
	(labels (
		 (l-star (x) (first (funcall (lattice-lower-covers lat) x)))
		 (u-star (x) (first (funcall (lattice-upper-covers lat) x))) )
		(dolist (a jis-2)
		 (dolist (q mis-2)
			 (when (and (lssql (car a) (u-star (car q)) lat)
				    (not (lssql (car a) (car q) lat)))
			       (setf (gethash a adj-ht) 
				     (cons q (gethash a adj-ht)))
			       (setf (gethash q rev-adj-ht) 
				     (cons a (gethash q rev-adj-ht))) )))
		(dolist (a jis-2)
		 (dolist (q mis-2)
			 (when (and (lssql (l-star (car a)) (car q) lat)
				    (not (lssql (car a) (car q) lat)))
			       (setf (gethash q adj-ht) 
				     (cons a (gethash q adj-ht)))
			       (setf (gethash a rev-adj-ht) 
				     (cons q (gethash a rev-adj-ht))) )))
		(multiple-value-bind 
		 (scc scc-ht count)
		 (scc elems 
		      #'(lambda (x) (gethash x adj-ht))
		      #'(lambda (x) (gethash x rev-adj-ht)))
		 (declare (ignore scc scc-ht))
		 (if (= count 1) 
		     t 
		     (values 
		      nil 
		      (format nil 
			  "There are ~d join irreducible congruences."
			  count)) ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;						    ;;;;;;
;;;;              Sudirect decompositions           ;;;;;; 
;;;						    ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This calculates the list of congruences that define that give
;;; the unique irredundant decomposition of L into si's.
;;; It calculates the atoms of Con L. It would make more sense for
;;; this to be done automatically by con and saved in a slot, but
;;; there are not more slots so I'll wait for the clos implementation.

(defun subdirect-decomposition (&optional (lat *lat*))
 (macrolet ((vec2congr (vec lat)
                `(let* ((th (create-congruence)))
		       (setf (congruence-lat th) ,lat)
		       (setf (congruence-vector th) ,vec)
		       th )))
  (let* ((con (con lat))
	 (jis (lattice-ji-list con))
	 (k (length jis))
	 (ht (make-hash-table-safe :size k))
	 (decomp nil)
	 (atoms nil))
	(dolist (x jis)
		(dolist (y jis)
			(if (and (not (eql x y))
				 (lssql x y con))
			    (setf (gethash y ht) t))))
	(dolist (x jis)
		(unless (gethash x ht)
			(push x atoms)))
	(dolist (x atoms) 
		(let* ((bv (make-zero-bitvector k))) 
		      (dolist (y jis) 
			(unless 
			 (lssql x y con) 
			 (bit-ior bv (congruence-vector y) bv)))
		      (push (vec2congr bv lat) decomp)))
	(values (nreverse decomp) atoms))))



;; 
;; A faster version is below.

;; dist-closure1(set) is the join closure of a set 
;; of join irreducibles in a 
;; distributive lattice. In this version the elements of the 
;; lattice are bit vectors, but the algorithm would work for
;; any distributive lattice.  Suppose set is (x_0, ... , x_{k-1}), 
;; then lst1 in close-aux will be an
;; antichain of the form (x_{i_0}, ..., x_{i_{s-1}}) with
;; i_0 > ... > i_{s-1}, lst2 is some tail of set, disjoint from 
;; lst1. join-lst1 is the join of lst1. The answer is push 
;; onto elements.

;(defun dist-closure1 (set)
;  (let* ((elements nil)
;	 (n (array-dimension (car set) 0))
;	 (bit-vec (make-zero-bitvector n))
;	 (bit-zero (make-zero-bitvector n)) )
;	(labels 
;	 ((close-aux (lst1 lst2 join-lst1)
;	   (if (null lst2)
;	       elements
;	       (progn
;		(unless (or (equal (bit-ior (car lst2) join-lst1 bit-vec)
;				   join-lst1)
;			    (above-one (car lst2) lst1))
;			;; need to make a new copy of the join
;			(push (bit-ior (car lst2) join-lst1) elements)
;			(close-aux (cons (car lst2) lst1)
;				   (cdr lst2) (car elements)))
;		(close-aux lst1 (cdr lst2) join-lst1)) ))
;	  (above-one (vec lst)
;		     (loop
;		      (if (null lst) (return nil))
;		      (if (equal vec (bit-ior vec (pop lst) bit-vec))
;			  (return-from above-one t)))) )
;	 (cons bit-zero (close-aux nil set bit-zero)) )))

;;; This is the new version; the one in the monograph. It runs
;;; in O(nN) time.  Using remove was a little slower than a 
;;; dolist.

;(defun dist-closure-bv (set)
;  (let* (
;	 (n (array-dimension (car set) 0))
;	 (bit-vec (make-zero-bitvector n))
;	 (bit-zero (make-zero-bitvector n))
;	 (elements (list bit-zero)) )
;	(labels 
;	 ((remove-comps (x lst)
;           (if (null lst)
;	       nil
;	       (if (or (equal (bit-ior x (car lst) bit-vec) x) 
;		       (equal (bit-and x (car lst) bit-vec) x))
;		   (remove-comps x (cdr lst))
;		   (cons (car lst) (remove-comps x (cdr lst))))))
;	  (close-aux (bv-anti bv-x lst2)
;	   (let* (			
;		  ;;(lst3 nil)
;		  (bv-anti2 (bit-ior bv-anti bv-x)))
;		 (push bv-anti2 elements)
;		 (when lst2
;		       (close-aux bv-anti (car lst2) (cdr lst2))
;		       (setf lst2 (remove-comps bv-x lst2))
;		       (if lst2
;			   (close-aux bv-anti2 (car lst2) (cdr lst2)))))))
;	 (close-aux bit-zero (car set) (cdr set))
;	 elements )))
;
;
;
