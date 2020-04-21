;;; chaindec.lisp   7/14/90

;;; A revision. 3/25/93
;;; Most most more care about not unnecessarily traversing lists.
;;; Time: chain-decompose L_6^n :
;;;
;;;	       n = 	10	15
;;;	     old	1.45	3.79
;;;	     new	0.59	1.26

;;; routines to find a Dilworth decomposition of a poset into
;;; chains, using a modification of the algorithm in Bogard's book, 
;;; "Introductory Combinatorics."

;;; 	o
;;;	|
;;;   a o o
;;;	 \|
;;;	  o c
;;;	  |\
;;;	b 0 0
;;;
;;; This P with the 3 vertical chains as a chin cover, shows that in 
;;; temp in chain-dec2 below we must use the whole ideal below elem,
;;; not just the lower covers. That is, the only place to start an
;;; alternating walk is  a  (for this chain decomposition) and the
;;; only choice for  b  that does any good is the one indicated, not
;;; the lower cover of  a.  (Well actually you could choose  b  to
;;; to be the lower cover, but then  c  would be on higher and only
;;; possible way to coninue is choose the bottom right element next,
;;; and it is not a lower cover of the new  c.)


;;;(in-package :user)

;;; lat can be a finite-poset or finite lattice or just a subset of 
;;; the elements of a poset. This returns two values
;;; a chain decomposition and a maximal sized antichain.
;;; It defines and then repeatedly call chain-dec2 which starts with
;;; a (current) chain decomposition, chains, which is a list of disjoint
;;; chains each having the order of the poset, ie, (a b c ...) with 
;;; a < b < c < ... . It does a breadth first search looking for an
;;; "alternating sequence." Starting with a minimal element, a_0,
;;; of one of the chains, it looks for an unused b_0 < a_0 and then goes to
;;; the upper cover  a_1  of  b_0  in its chain.  b_0  is marked used
;;; (used[b_0] is set to t) and pair[a_1] is set to (a_0 b_0) and
;;; the process continues with  a_1.  If
;;; b_0 is the top of its chain (so  a_1  does not exist) then an alternating
;;; decreasing walk has been found and redo-chains is called to make a 
;;; smaller chain decomposition. If no such walk is found, then chains
;;; is a minimal sized chain decomposition.
;;; See the monograph for a more detailed explaination.

(defun chain-decompose (&optional (lat *lat*) (elems (lattice-elem-list lat)))
  (let* 
   ((n (length elems))
    (lq (lattice-lssql lat))
    ;; ideals-ht[x] will be the elements strictly below x.
    (ideals-ht (make-hash-table-safe :size n))
    ;; tail[x] is the tail of the chain with x starting at x.
    (tail (make-hash-table-safe :size n))
    (chains (mapcar #'list elems))
    (used (make-hash-table-safe :size n))
    ;; for a used pair (a b), pair[c] = (a b)
    (pair (make-hash-table-safe :size n)) )
   (dolist (x elems)
	   (dolist (y elems)
		   (if (and (not (eq x y)) (lssql x y lat))
		       (setf (gethash y ideals-ht)
			     (cons x (gethash y ideals-ht))))))
   (dolist (x chains)
	   (setf (gethash (car x) tail) x))
   (labels 
    ((chain-dec2 ()
       (clrhash used)
       (clrhash pair)
       (let*
	((new (mapcar #'car chains))
	 (maxs new)     ; maxs will be  a maximal sized antichain
	 (new2 nil) )
	(loop
	 (loop
	  (if (null new) (return))
	  (let* ((a (car new))
		 (temp nil) )
		(dolist (x (gethash a ideals-ht))
			(unless (gethash x used)
				(push x temp)))
		(loop
		 (if (null temp) (return))
		 (let* (
			(b  (pop temp))
			(tail (gethash b tail))
			;; exists? tells if b is not the top of its chain
			(exists? (cdr tail))
			succ )
		       (if exists?
			   (progn
			    (setf succ (second tail))
			    (setf (gethash succ pair) (list a b))
			    (setf (gethash b used) t)
			    (push succ maxs)
			    (push succ new2) )
			   (return-from chain-dec2
					(redo-chains a b))) )) )
	  (pop new) )
	 (if new2
	     (setq new new2 new2 nil)
	     (return (values chains (mxl maxs lq)))) ) ) )
      
     (redo-chains (a b )
       ;;extend the chain ending with b to the tail of the chain with a.
       (setf (cdr (gethash b tail)) (gethash a tail))
       (let* ((pair (gethash a pair)))
	     (if pair
		 (redo-chains (first pair) (second pair) )
		 (remove (gethash a tail) chains)))) )
    (loop
     (multiple-value-bind (chains2 antichain)
			  (chain-dec2)
			  (if (eql chains chains2)
			      (return (values chains antichain))
			      (setf chains chains2) ))) )))

;;; This makes the lattice of all maximum sized antichains. lat can be
;;; a poset. See the monograph for a description of the algorithm.

(defun maximum-sized-antichains (&optional 
				 (lat *lat*) (elems (lattice-elem-list lat))
				 (list-only t))
  (if list-only
      (warn "This gives the list of antichains. To actually make the~%~
	    lattice of all maximum sized antichains, the third arguemnt~%~
	    must be nil."))
  (let* 
   ((n (length elems))
    ;; ideals-ht[x] will be the elements strictly below x.
    (ideals-ht (make-hash-table-safe :size n))
    ;; tail[x] is the tail of the chain with x starting at x.
    (tail (make-hash-table-safe :size n))
    (chains (chain-decompose lat elems))
    (chains2 chains)
    (ch-ht (make-hash-table-safe :size n))
    (k (length chains))
    (ji-list nil)
    zero
    (used (make-hash-table-safe :size n)) )
   (dotimes (i k)
	    (let ((ch (pop chains))) 
		 (dolist (y ch)
			 (setf (gethash y ch-ht) i))))
   (setf chains chains2)
   (dolist (x elems)
	   (dolist (y elems)
		   (if (and (not (eq x y)) (lssql x y lat))
		       (setf (gethash y ideals-ht)
			     (cons x (gethash y ideals-ht))))))
   (dolist (chain chains)
	   (loop
	    (if (null chain)
		(return)
		(progn 
		 (setf (gethash (car chain) tail) chain)
		 (pop chain)))))
   ;; should pass this stuff to chain-decompose
   (labels 
    ((xbar (x)
       (clrhash used)
       (setf chains (copy-list chains2))
       (setf (nth (gethash x ch-ht) chains) (gethash x tail))
       (let*
	((new (mapcar #'car chains))
	 (vec (make-array k))
	 (chains3 chains)
	 (new2 nil) )
	(dotimes (i k) 
		 (setf (aref vec i) (car (pop chains3))))
	(loop
	 (loop
	  (if (null new) (return))
	  (let* ((a (car new))
		 (temp (gethash a ideals-ht)) )
		(loop
		 (if (null temp) (return))
		 (if (or (gethash (car temp) used) 
			 (and 
			  (= (gethash (car temp) ch-ht) (gethash x ch-ht))
			  (not (eq (car temp) x)) 
			  (lssql (car temp) x lat)))
		  (pop temp)
		  (let* (
			(b  (pop temp))
			(tail (gethash b tail))
			;; exists? tells if b is not the top of its chain
			(exists? (cdr tail))
			succ )
		       (if exists?
			   (progn
			    (setf succ (second tail))
			    (setf (gethash b used) t)
			    (if (lssql (aref vec (gethash succ ch-ht))
				       succ lat) 
				(setf (aref vec (gethash succ ch-ht)) succ))
			    (push succ new2) )
			   (return-from xbar nil)) )) ))
	  (pop new) )
	 (if new2
	     (setq new new2 new2 nil)
	     (if (eq x (aref vec (gethash x ch-ht))) 
		 (return-from xbar vec)
		 (return-from xbar nil)) ) ) ))
     (join2 (x y) 	; join of 2 antichains, represented as lists
	    (if (null x)
		nil
		(if (lssql (car x) (car y) lat)
		    (cons (car y) (join2 (cdr x) (cdr y)))
		    (cons (car x) (join2 (cdr x) (cdr y))) )))
     (meet2 (x y) 	; join of 2 antichains, represented as lists
	    (if (null x)
		nil
		(if (lssql (car x) (car y) lat)
		    (cons (car x) (meet2 (cdr x) (cdr y)))
		    (cons (car y) (meet2 (cdr x) (cdr y))) )))
     (leq (x y)
	  (if (null x)
	      t
	      (if (lssql (car x) (car y) lat)
		  (leq (cdr x) (cdr y))
		  nil)))
     )	;end of labels
    (dolist (x elems)
            (setf chains chains2)
	    (let ((vec (xbar x)))
		 (if vec (pushnew vec ji-list :test #'equalp))))
    (setf ji-list (mapcar #'(lambda (vec) (map 'list #'identity vec)) ji-list))
    (setf zero (reduce #'meet2 ji-list))
    (if list-only
     (dist-closure (remove zero ji-list :test #'equal) zero #'leq #'join2)
     (make-finite-lattice-from-lssql 
       (dist-closure (remove zero ji-list :test #'equal) zero #'leq #'join2)
       #'leq))
      )))

;;; This is the new version; the one in the monograph. It runs
;;; in O(nN) time.  Using remove was a little slower than a 
;;; dolist.

(defun dist-closure (set zero leq join2)
 (if (null set)
  (list zero)
  (let* (
	 (elements (list zero)) )
	(labels 
	 ((remove-comps (x lst)
           (if (null lst)
	       nil
	       (if (or (funcall leq x (car lst)) 
		       (funcall leq (car lst) x))
		   (remove-comps x (cdr lst))
		   (cons (car lst) (remove-comps x (cdr lst))))))
	  (close-aux (join-anti x lst2)
	   (let* ((join-anti2 (funcall join2 join-anti x)))
		 (push join-anti2 elements)
		 (when lst2
		       (close-aux join-anti (car lst2) (cdr lst2))
		       (setf lst2 (remove-comps x lst2))
		       (if lst2
			   (close-aux join-anti2 (car lst2) (cdr lst2)))))))
	 (close-aux zero (car set) (cdr set))
	 elements ))))


;;; ------------------  Quick Version  ----------------------------
;;;
;;; This quickly finds a chain decomposition, which in generaly has more
;;; chains than the width but the expected number is still not too
;;; great. It is based on Mehlhorn: "Data Structures and algorithms 2"
;;; page 12.
;;;
;;; The chains constructed are all cover-preserving, so, for example,
;;; the poset that looks like an X must have three chains, rather than
;;; two.
;;;
;;; upper-covers is a list whose elements look like (a upper-covers-of-a).
;;; It assumes that this list has been topologically sorted.
;;; It returns two values: a hash table mapping each element to an 
;;; integer < k (indicated the number of hte chain it is in) and 
;;; k (= the number of chains).

(defun quick-chain-dec (upper-covers)
  (let* (
	 (n (length upper-covers))
	 (h -1)				; number of the current chain
         (ht-uc (make-hash-table-safe :size n :test #'equal))
         (ht-ch (make-hash-table-safe :size n :test #'equal)) )
        (mapc #'(lambda (x) (setf (gethash (car x) ht-uc) (second x)))
              upper-covers)
        (loop
         (loop
          (if (null upper-covers) (return-from quick-chain-dec 
					       (values ht-ch (1+ h))))
          (if (multiple-value-call #'(lambda (x y)
                                             (declare (ignore x))
                                             (not y))
                                  (gethash (first (car upper-covers)) ht-uc))
             (pop upper-covers)
             (return) ) )
         (let* (
                (elt (first (pop upper-covers)))
                (uc-elt (gethash elt ht-uc))
		(h (incf h)))
	       (setf (gethash elt ht-ch) h)
               (remhash elt ht-uc)
               ;; Note this loop is strange because elt and uc-elt
               ;; change as it runs.
               (loop
                (if (null uc-elt) (return))
                (multiple-value-bind
                 (uc2 found-p) (gethash (car uc-elt) ht-uc)
                 (if (not found-p)
                     (pop uc-elt)
                     (progn
		      (setf (gethash (car uc-elt) ht-ch) h)
                      (remhash (car uc-elt) ht-uc)
                      (setq elt (car uc-elt))
                      (setq uc-elt uc2)))))))))


;;; The old version, which actually returned the chains as lists.

;(defun quick-chain-dec (upper-covers)
;  (let* (
;	 (chains nil)
;	 (ht (make-hash-table :size (length upper-covers) :test #'equal)))
;	(mapc #'(lambda (x) (setf (gethash (car x) ht) (second x))) 
;	      upper-covers) 
;	(loop
;	 (loop
;	  (if (null upper-covers) (return-from quick-chain-dec chains))
;	  (if (multiple-value-call #'(lambda (x y) 
;					     (declare (ignore x))
;					     (not y)) 
;				  (gethash (first (car upper-covers)) ht))
;	     (pop upper-covers)
;	     (return) ) )
;	 (let* (
;		(elt (first (pop upper-covers)))
;		(uc-elt (gethash elt ht))
;		(chain (list elt)))
;	       (remhash elt ht)
;	       ;; Note this loop is strange because elt and uc-elt
;	       ;; change as it runs.
;	       (loop 
;		(if (null uc-elt) (return (push (nreverse chain) chains))) 
;		(multiple-value-bind 
;		 (uc2 found-p) (gethash (car uc-elt) ht)
;		 (if (not found-p) 
;		     (pop uc-elt)
;		     (progn 
;		      (push (car uc-elt) chain)
;		      (remhash (car uc-elt) ht)
;		      (setq elt (car uc-elt)) 
;		      (setq uc-elt uc2)))))))))


;;; ---------------------  testing stuff  ----------------------
; (setq two (make-finite-poset '(a b) '( (a (b)) (b nil) )))
; (setq three (make-finite-poset '(a b c) '( (a (b)) (b (c)) (c nil) )))
; (setq n (make-finite-poset '(a b c d) '( (a (b)) (b nil) (c (b d)) (d nil))))

; (defun c-d (poset) (chain-decompose poset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Antichain decomposition (rather trivial).
;;
;; Add a key from-top

(defun antichain-decompose (&optional (lat *lat*) 
				      (elems (lattice-elem-list lat)))
  (antichain-decompose0 (if elems elems (lattice-elem-list lat))
		        (lattice-grtrql lat)) )

(defun antichain-decompose0 (elems geq &optional ans)
  (cond 
   ((null elems) (nreverse ans))
   (t (multiple-value-bind (mins elts) (mnl elems geq)
			  (antichain-decompose0 elts geq (cons mins ans))))))


(defun dimension (&optional (lat *lat*) (elems (lattice-elem-list lat)))
  (1- (length (antichain-decompose lat elems))))

(defun width (&optional (lat *lat*) (elems (lattice-elem-list lat)))
  (length (chain-decompose lat elems)))
