; finstructnew.lisp     9/27/92
; a revision of 
; finsturct.lisp (C) Ralph Freese (3/1/89)


;    *******   Finite Lattices   ***********

;;;(in-package :user)

(defparameter *ht-factor* 1.5)

;;; Note we have deleted elems from the arglist which means all *.lat files
;;; and everything that refers to this will have to be changed.
;;; The optional filters argument is for the case that the filters have 
;;; been already calculated, eg., make-finite-lattice-from-grtrql.
;;; filters should be topologically sorted.

(defun make-finite-lattice (upper-covers &optional (check t) filters ht-ucovs)
  ;; put in a check to make sure there are no duplicates and
  ;; nothing is an upper-cover to itself.
  ;; issue a warning with (cerror)
  (let* (
      (temp (create-lattice))
      (upper-covers (topological-sort upper-covers))
      join-table
      meet-table
      le
      (n (length upper-covers))
      (ht-ord (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      ;; only make ht-uc if ht-ucovs is nil.
      (ht-uc (unless ht-ucovs
	      (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal)))
      (ht-le (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (uc2 upper-covers)
      (upper-covers (remove-duplicates upper-covers :key #'car :test #'equal))
      (elems (mapcar #'first upper-covers)) )
    ;; Improve this to give the duplicate.
    (if (not (equal upper-covers uc2))
      (cerror "Remove the duplicates."
        "The list of elements contained duplicates.~%") )
    (setf (lattice-type temp) 'finite-lattice)
    (setf (lattice-elem-list temp) elems)
    (setf (lattice-upper-covers-list temp) upper-covers)
    (if ht-uc
        (mapc #'(lambda (x) 
		 (setf (gethash (car x) ht-uc) (second x))) upper-covers))
    (if ht-ucovs (setf ht-uc ht-ucovs))
    (let* ((k -1)) 
	  (dolist (x upper-covers)
		  (setf (gethash (car x) ht-ord) (incf k))))
    (setf (lattice-upper-covers temp)
      #'(lambda (x) (gethash x ht-uc)) )
    ;; note: deleted elems from arg to make-filters
    (setf (lattice-filters temp) (setq filters 
				       (if filters filters
					   (make-filters upper-covers))))
    (mapc #'(lambda (x) 
             (let* ((ht (setf (gethash (car x) ht-le) 
			      (make-hash-table 
			       :size 
			       (floor (* *ht-factor* (length (second x)))) 
			       :test #'equal))))
		   (mapc #'(lambda (y) (setf (gethash y ht) t)) (second x)) ))
	  filters)
    (setf (lattice-lssql temp) (setq le
      #'(lambda (x y)
	 (values (gethash y (gethash x ht-le)))) ))
    (setf (lattice-grtrql temp)
      #'(lambda (x y)
	 (funcall le y x) ) )
    (setf (lattice-ideals temp) (make-ideals filters))
    (multiple-value-bind 
     (lc ht-lc) (make-ideals upper-covers) 
     (setf (lattice-lower-covers-list temp) lc)
     (setf (lattice-lower-covers temp) 
	   #'(lambda (x) (gethash x ht-lc)) ) )
    ;; The join and meet tables are now a hash tables. 
    (setf join-table
	  (make-join-meet-table 'join (reverse filters) 
				(lattice-upper-covers temp) le ht-ord check)
          (lattice-join-table temp) join-table )
    (setf (lattice-join temp) 
	  #'(lambda (args) 
		    (if (null args) 
			(lattice-zero temp)
			(reduce #'(lambda (x y) 
					  (gethash y (gethash x join-table)))
				args))))
    (setf meet-table
	  (make-join-meet-table 'meet (lattice-ideals temp) 
				(lattice-lower-covers temp) 
				(lattice-grtrql temp) ht-ord nil )
          (lattice-meet-table temp) meet-table )
    (setf (lattice-meet temp) 
	  #'(lambda (args) 
		    (if (null args) 
			(lattice-one temp)
			(reduce #'(lambda (x y) 
					  (gethash y (gethash x meet-table)))
				args))))
    (setf (lattice-zero temp)
          (funcall (lattice-meet temp) elems) 
	  (lattice-one temp) 
	  (funcall (lattice-join temp) elems) 
	  (lattice-ji-list temp) 
          (remove-if-not #'(lambda (x) (eql 
					(length 
					 (funcall 
					    (lattice-lower-covers temp) 
					    x)) 1))
			 elems)
	  (lattice-mi-list temp) 
          (remove-if #'(lambda (x) 
			 (let* ((uc (funcall (lattice-upper-covers temp) x)))
			       (or (null uc) (cdr uc))))
		     elems) )
    temp) )

;;; Jarda's algorithm for finding the upper covers from leq.
;;; It runs in time O(n^2).

;;; The permutation of the x y z below is because upper-covers-from-grtrql
;;; return two values, which are to be the first and third argument to
;;; make-finite-lattice.

(defun make-finite-lattice-from-grtrql (elems geq-fn &optional (check t))
  (let* ((tmp (upper-covers-and-filters-from-grtrql elems geq-fn)))
	(make-finite-lattice (first tmp) check (second tmp))))

(defun upper-covers-and-filters-from-grtrql (elems geq-fn)
  (let* ((ht (make-hash-table :size (floor (* *ht-factor* (length elems))) 
			      :test #'equal))
	 (upper-covers-list nil)
	 elems2
	 filters)
	(dolist (x elems) 
		(dolist (y elems) 
			(if (funcall geq-fn y x) 
			    (setf (gethash x ht) (cons y (gethash x ht))))))
	(setq filters (topological-sort
		       (mapcar #'(lambda (x) (list x (gethash x ht))) elems)))
	(setq elems (mapcar #'first filters)
	      elems2 elems)
	(dolist (a elems)
		(pop elems2)
		(let* ((uc-a nil))
		      (dolist (x elems2)
			      (if (funcall geq-fn x a)
			       (let* ((uc-a2 uc-a)) 
				     (loop 
				      (when (null uc-a2)
					    (push x uc-a)
					    (return))
				      (if (funcall geq-fn x (pop uc-a2))
					  (return)) ))))
		      (push (list a uc-a) upper-covers-list)))
	(list upper-covers-list filters) ) )

(defun upper-covers-from-lssql (elems leq-fn) 
  (upper-covers-and-filters-from-grtrql elems 
	#'(lambda (x y) (funcall leq-fn y x))))

;;; *********************  The old version ****************

;;; This uses fn (grtrql) to make a hash table of filters, ht. Then
;;; sorts these topologically. Then it uses the O(n(\ecov +1))
;;; algorithm of the monograph to find the upper covers. Then it
;;; calls make-finite-lattice.
;;; On the join meet closure of '(a b c d), a lattice isomorphic to 
;;; FD(4), this took 2 min. 20 secs. The previous version, which 
;;; calculated the ideals in 1/x and took those elems whose ideal had
;;; size 2, took 3 min. 15 secs.

;(defun make-finite-lattice-from-grtrql (elems fn &optional (check t) 
;					      &aux filters upper-covers)
;  ;; fn is the grtrql function
;  (let* ((ht (make-hash-table :size (floor (* *ht-factor* (length elems))) 
;			      :test #'equal)) 
;	 (ht-uc (make-hash-table 
;		 :size (floor (* *ht-factor* (length elems))) :test #'equal))
;	 (ht-non-cov-p (make-hash-table 
;		 :size (floor (* *ht-factor* (length elems))) :test #'equal)))
;	(dolist (x elems) 
;		(dolist (y elems) 
;			(if (funcall fn y x) 
;			    (setf (gethash x ht) (cons y (gethash x ht))))))
;	(setq filters (topological-sort
;		       (mapcar #'(lambda (x) (list x (gethash x ht))) elems)))
;	(setq elems (nreverse (mapcar #'first filters)))
;	(dolist (x elems) 
;		(clrhash ht-non-cov-p)
;		(dolist (y (gethash x ht))
;			(if (not (equal y x)) 
;			    (dolist (z (gethash y ht-uc))
;				    (setf (gethash z ht-non-cov-p) t))))
;		(dolist (y (gethash x ht))
;			(if (not (equal y x)) 
;			    (unless (gethash y ht-non-cov-p)
;				    (setf (gethash x ht-uc)
;					  (cons y (gethash x ht-uc)))))))
;	(dolist (x elems)
;		(push (list x (gethash x ht-uc)) upper-covers))
;	(make-finite-lattice upper-covers check filters ht-uc) ) )

(defun make-finite-lattice-from-lssql (elems fn &optional (check t))
  (make-finite-lattice-from-grtrql 
   elems #'(lambda (x y) (funcall fn y x)) check))

(defun make-finite-semilattice (upper-covers &optional (check t))
  ;; put in a check to make sure there are no duplicates and
  ;; nothing is an upper-cover to itself.
  ;; issue a warning with (cerror)
  (let* (
      (temp (create-lattice))
      le
      meet-table
      (len (length upper-covers))
      (upper-covers (topological-sort 
	    (remove-duplicates upper-covers :test #'equal :key #'car)))
      (n (length upper-covers))
      (join-table ;			; for the partial join operation
       (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (ht-uc (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (ht-le (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (ht-ord (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (elems (mapcar #'first upper-covers)) )
    (if (not (= n len))
      (cerror "Remove the duplicates."
        "The list of upper covers contained duplicates elements.~%") )
    (setf (lattice-type temp) 'finite-semilattice)
    (setf (lattice-elem-list temp) elems)
    (setf (lattice-upper-covers-list temp) upper-covers)
    (mapc #'(lambda (x) (setf (gethash (car x) ht-uc) (second x))) upper-covers)
    (let* ((k -1))
          (dolist (x upper-covers)
                  (setf (gethash (car x) ht-ord) (incf k))))
    (setf (lattice-upper-covers temp)
		#'(lambda (x) (gethash x ht-uc)) )
    (setf (lattice-filters temp) (make-filters upper-covers))
    (mapc #'(lambda (x) 
	     (let* ((ht (setf (gethash (car x) ht-le) 
			      (make-hash-table 
			       :size 
			       (floor (* *ht-factor* (length (second x)))) 
			       :test #'equal)))) 
		   (mapc #'(lambda (y) (setf (gethash y ht) t)) (second x)) )) 
	  (lattice-filters temp))
    (setf (lattice-lssql temp) (setq le
      #'(lambda (x y)
         (values (gethash y (gethash x ht-le)))) ))
    (setf (lattice-grtrql temp)
      #'(lambda (x y)
         (funcall le y x)))
    (setf (lattice-ideals temp) (make-ideals (lattice-filters temp)))
    (multiple-value-bind
     (lc ht-lc) (make-ideals upper-covers)
     (setf (lattice-lower-covers-list temp) lc)
     (setf (lattice-lower-covers temp)
           #'(lambda (x) (gethash x ht-lc)) ) )
    (setf meet-table
          (make-join-meet-table 'meet (lattice-ideals temp)
                                (lattice-lower-covers temp)
                                (lattice-grtrql temp) ht-ord check )
          (lattice-meet-table temp) meet-table )
    (setf (lattice-meet temp)
          #'(lambda (args)
                    (if (null args)
                        (error "The meet of the empty set is not define for ~
					semilattices")
                        (reduce #'(lambda (x y)
                                          (gethash y (gethash x meet-table)))
                                args))))
    (setf (lattice-zero temp) (funcall (lattice-meet temp) elems))
    (setf (lattice-ji-list temp) 
          (remove-if #'(lambda (x) (not (eql (length (the list
                         (second (assoc x (lattice-lower-covers-list temp)
                                        :test #'equal)))) 1)))
                     elems))
    (setf (lattice-mi-list temp) 
          (remove-if #'(lambda (x) (not (eql (length (the list
                         (second (assoc x (lattice-upper-covers-list temp)
                                        :test #'equal)))) 1)))
                     elems))
    ;; This will make the partal join table. It values at x y will be 
    ;; (cons x+y t) if x+y exists, else (cons nil nil).
    (dolist (x elems)
     (setf (gethash x join-table) 
	   (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal)))
    (let* ((filters (lattice-filters temp)))
	  (loop
	   (if (null filters) (return))
	   (setf (gethash (caar filters) (gethash (caar filters) join-table))
		 (cons (caar filters) t))
	   (let ((filters2 (cdr filters)))
		(loop
		 (if (null filters2) (return))
		 (let ((filter (second (car filters)))
		       (join nil)
		       (exists? nil) )
		      (loop
		       (when (null filter) 
			     (if exists?
				 (setf 
				  (gethash (caar filters) 
				   (gethash (caar filters2) join-table))
				  (cons join t)
				  (gethash (caar filters2) 
				   (gethash (caar filters) join-table))
				  (cons join t))
				 (setf 
				  (gethash (caar filters) 
				   (gethash (caar filters2) join-table))
				  (cons nil nil)
				  (gethash (caar filters2) 
				   (gethash (caar filters) join-table))
				  (cons nil nil)) )
			     (return))
		       (when (funcall le (caar filters2) (car filter))
			     (if exists?
				 (setf join (funcall (lattice-meet temp)
					      (list join (car filter))))
				 (setf exists? t
				       join    (car filter))))
		       (pop filter) ))
		 (pop filters2) ))
	   (pop filters) ))
    (setf (lattice-join-table temp) join-table)
    (labels ((join (args)
                   (if (null args)
                       (cons (lattice-zero temp) t)
		       (let* ((join-rest (join (cdr args))))
			     ;; join-rest is either (join . t) or (nil . nil)
			     (if (cdr join-rest) 
				 (gethash (car join-rest) 
				  (gethash (car args) join-table))
				 join-rest)))))
	    (setf (lattice-join temp) #'(lambda (lst) 
						(let ((join (join lst)))
						     (values (car join)
							     (cdr join))))))
    temp) )


(defun make-finite-poset (upper-covers &optional filters)
  (let* (
      (temp (create-lattice)) 
      le
      (upper-covers (topological-sort upper-covers))
      (n (length upper-covers))
      (ht-ord (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (ht-uc (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (ht-le (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal))
      (upper-covers (remove-duplicates upper-covers :key #'car :test #'equal))
      (elems (mapcar #'first upper-covers)) )
    (if (not (= n (length upper-covers)))
      (cerror "Remove the duplicates."
        "The list of elements contained duplicates.~%") )
    (setf (lattice-type temp) 'finite-poset)
    (setf (lattice-elem-list temp) elems)
    (setf (lattice-upper-covers-list temp) upper-covers)
    (mapc #'(lambda (x) (setf (gethash (car x) ht-uc) (second x))) upper-covers)    (let* ((k -1))
          (dolist (x upper-covers)
                  (setf (gethash (car x) ht-ord) (incf k))))
    (setf (lattice-upper-covers temp)
      #'(lambda (x) (gethash x ht-uc)) )
    (setf (lattice-filters temp) (if filters filters
					   (make-filters upper-covers)))
    (mapc #'(lambda (x)
             (let* ((ht (setf (gethash (car x) ht-le)
                              (make-hash-table
                               :size 
			       (floor (* *ht-factor* (length (second x)))) 
			       :test #'equal))))
                   (mapc #'(lambda (y) (setf (gethash y ht) t)) (second x)) ))
          (lattice-filters temp))
    (setf (lattice-lssql temp) (setq le
      #'(lambda (x y)
         (values (gethash y (gethash x ht-le)))) ))
    (setf (lattice-grtrql temp)
      #'(lambda (x y)
         (funcall le y x) ) )
    (setf (lattice-ideals temp) (make-ideals (lattice-filters temp)))
    temp) )


(defun add-cover-stuff-finite-lattice (&optional (temp *lat*) &aux elems)
    (setq elems (lattice-elem-list temp))
    (setf (lattice-join-covers-list temp)
      (make-join-covers-list 
       elems (lattice-ji-list temp) temp (lattice-lower-covers temp)))
    (setf (lattice-join-covers temp) ; now only for join irreds
      #'(lambda (x)
        (second (assoc x (lattice-join-covers-list temp) :test #'equal))))
    (setf (lattice-meet-covers-list temp)
      (make-meet-covers-list 
       elems (lattice-mi-list temp) temp (lattice-upper-covers temp)))
    (setf (lattice-meet-covers temp) ; now only for meet irreds
      #'(lambda (x)
        (second (assoc x (lattice-meet-covers-list temp) :test #'equal))))
    (setf (lattice-j-list temp)
      (make-j-list (lattice-ji-list temp) temp) )
    (setf (lattice-j temp)
      #'(lambda (x)
        (second (assoc x (lattice-j-list temp) :test #'equal))))
    (setf (lattice-j-dual-list temp)
      (make-j-dual-list (lattice-mi-list temp) temp) )
    (setf (lattice-j-dual temp)
      #'(lambda (x)
        (second (assoc x (lattice-j-dual-list temp) :test #'equal))))
    temp)


;;; upper-covers-list must be sorted (on the first) by a topological
;;; sort. 
;;; This is based on K. Simon's improvement of Goralcikova and Koubek's
;;; algorithm, see Chapter 11 of our monograph on free lattices. See also
;;; Mehlhorn p. 15. We have followed the
;;; notation in the algorithm presented in the monograph..
;;; g (h x)  is the list of elements in chain h above x.

(defun make-filters (upper-covers-list)
  (multiple-value-bind 
   (ht-ch k) (quick-chain-dec upper-covers-list)
   (let* ((g-array (make-array k)) 
	  (n (length upper-covers-list))
	  (i n)
	  (uc-rev (reverse upper-covers-list))
	  (ht-ord (make-hash-table-safe 
		   :size (floor (* n *ht-factor*)) :test #'equal)) )
	 ;; initialize the hash table for ORD
	 (dolist (x uc-rev) 
		  (setf (gethash (car x) ht-ord) (decf i))) 
	 ;; initialize each entry of  g-array  to be a hash table. 
	 (dotimes (h k) (setf (svref g-array h) 
			      (make-hash-table 
			       :size (floor (* n *ht-factor*)) :test #'equal)))
	 (labels ( 
		  (ord (x) (gethash x ht-ord))
		  (ch (x) (gethash x ht-ch))
		  (g (h x) (gethash x (svref g-array h))) )
		 (dolist (uc uc-rev)		; uc has form (a (uc-of-a))
		  (let* ((x (first uc)))
			(dolist 
			 (y (second uc))
			 (let* ((g-ch-y-x (g (ch y) x)))
			       (if (or (null g-ch-y-x)
				       (< (ord y) (ord (first g-ch-y-x))))
				   ;; the above if will always succeed when 
				   ;; uc is just the upper covers. Having it
				   ;; forces the algorithm to use only the 
				   ;; upper covers, even if uc has some 
				   ;; additional elements. 
				   (dotimes 
				    (h k) 
				    (if (g h y) 
					(if (or (not (g h x))
						(< (ord (first (g h y))) 
						   (ord (first (g h x)))))
					    (setf 
					     (gethash x (svref g-array h))
					     (g h y))))))))
			(setf (gethash x (svref g-array (ch x))) 
			      (cons x (g (ch x) x))) ))
		 (mapcar #'(lambda (uc) 
			    (let* ((elem (car uc)) 
				   (filter nil)) 
				  (dotimes 
				   (h k) 
				   (setq filter (append (g h elem) filter)))
				  (list elem filter)))
			 upper-covers-list) ) ) ) )

 

;;; The elements of filters have the form (a filter-of-a). This 
;;; function is also called with `filters' being upper-covers, 
;;; yielding the lower covers and its hash table.

(defun make-ideals-ht (filters)
  (let* ((ht (make-hash-table-safe :size  
			      (floor (* *ht-factor* (length filters))) 
			      :test #'equal)))
	(mapc #'(lambda (x) 
		 (mapcar #'(lambda (y)
			    (setf (gethash y ht) 
				  (cons (car x) (gethash y ht))))
			 (second x)) )
	      filters)
	ht ) )

(defun make-ideals (filters)
  (let* ((ht (make-ideals-ht filters)))
	(values (mapcar #'(lambda (x) (list (car x) (gethash (car x) ht))) 
			filters)
		ht) ))

;;; This is based on the algorithm in Goralcik, Goralcikova, and Koubek, 
;;; "Fast recognition of rings and lattices." Filters is a list of pairs
;;; (a filter-of-a), topologically sorted (ie. linear extension). For joins,
;;; we reverse the list so that all elements above  a  are earlier in
;;; this list. For each x, we calculate x join z where z comes before x.
;;; To do this it suffices to find the minimun of y \j z for y \covs x,
;;; and these joins have already been found and the topology order make 
;;; it easy to find the minumun. The same function is used to calculate
;;; the meet hash table, but is harder to read.
;;;
;;; type must be one of the symbols join or meet.

(defun make-join-meet-table (type filters-rev upper-covers-fn le ht-ord check-p)
  (let* ((n (length filters-rev))
	 (done nil)
	 (ht (make-hash-table :size (floor (* n *ht-factor*)) :test #'equal)))
	(if (and check-p (funcall upper-covers-fn (caar filters-rev)))
	    (error "This should not happen. ~S has upper covers."
		   (caar filters-rev)))
	(labels ((j (x y) (gethash y (gethash x ht)))
		 (comp (x y) (if (eq type 'join) (< x y) (> x y)))
		 (ord (x) (gethash x ht-ord)))
	 (dolist (filter-pair filters-rev)
		 (let* ((htx (make-hash-table :size (floor (* n *ht-factor*)) 
					      :test #'equal))
		        (x (car filter-pair)) )
		       (setf (gethash x ht) htx) 
		       ;; The next line sets a \j a = a.  
		       (setf (gethash x htx) x)
		       (dolist (z done)
		        (let* ((joins (mapcar #'(lambda (y) (j y z)) 
					      (funcall upper-covers-fn x)))
			       (min (car joins))
			       (ord-min (ord min)))
			      (dolist (j (cdr joins))
				      (if (comp (ord j) ord-min)
					  (setq min j ord-min (ord j))))
			      (if check-p 
				  (dolist (w joins)
					  (if (not (funcall le min w))
					      (error
       "This is not a lattice!!~%~S and ~S do not have an ~a." 
       x z (if (equal type 'join) "upper bound" "lower bound")))))
			      (setf (gethash z htx) min 
				    (gethash x (gethash z ht)) min) )) 
		       (push x done)))) 
	ht ) )

;;;;;;;;;;;;;;;;; Join and Meet Covers ;;;;;;;;;;;;;;;;;;;

;;; 12/21/93  I am changing this so that lower-cover-fn is passed rather 
;;; than extract from lat because we are now using this with lat
;;; a fp lattice and (lattice-lower-covers lat) is not defined 
;;; when this is called.

(defun make-join-covers-list (elems ji-elems lat lower-covers-fn)
  (mapcar #'(lambda (x)
      (list x (make-join-covers x 
				ji-elems 
				(lattice-lssql lat)
				(lattice-join lat)
				lower-covers-fn )) ) 
	  elems) )

;;; Note this just calls make-join-covers with the dual objects.
;;; I.e., make-join-covers actually works for both join and meet covers.

(defun make-meet-covers-list (elems mi-elems lat upper-covers-fn)
  (mapcar #'(lambda (x)
      (list x (make-join-covers x 
				mi-elems 
				(lattice-grtrql lat)
				(lattice-meet lat)
				upper-covers-fn)) ) 
	  elems) )

;;; This is a greatly improved version. Rather than checking all
;;; subsets, it does not try any superset of a join redundant set
;;; nor any superset of a join-cover. For example for M_12 the old
;;; version took 3.25 minutes, the new 3 secs.
;;; I replaced the join irredundant test with a join minimal test,
;;; i.e., S must be a minimal join representation of \Join S.


(defun make-join-covers (elt0 jis le join lower-covers-fn)
  (let* ((ht-joins (make-hash-table :test #'eq))
	 (jis (remove-if #'(lambda (x) (funcall le elt0 x)) jis))
	 (join-covers nil) )
        (setf (gethash nil ht-joins) (funcall join nil))
	(labels 
	 (
	  (lc (x) 
           ;; x will be ji, so the first is its lower cover.
	   (first (funcall lower-covers-fn  x)))
	  ;; This tests if (cons elt lst) is join minimal.
	  (join-minimal-p (lst elt) 
	   (let* ((full-join (funcall join (list elt (gethash lst ht-joins))))) 
		 (if (equal full-join (funcall 
				       join (list 
					     (lc elt) (gethash lst ht-joins))))
		     nil 
		     (join-minimal-p-aux lst elt full-join))))
	  (join-minimal-p-aux (lst elt full-join) 
	   (if (null lst) 
	       t 
	       (if (equal full-join (funcall join 
					     (list 
					      elt (lc (car lst)) 
					      (gethash (cdr lst) ht-joins))))
		   nil
		   (join-minimal-p-aux 
		    (cdr lst) (funcall join (list elt (car lst))) full-join))))
	  (nonrefinable-p (lst elt)	; join is the join of set
	   (if (funcall le elt0 (funcall join (list (lc elt) 
						    (gethash lst ht-joins))))
	       nil
	       (nonrefinable-aux lst elt)))
	  (nonrefinable-aux (lst elt)
	   (if (null lst) 
	       t 
	       (if (funcall le 
                    elt0
		    (funcall join (list (lc (car lst)) 
				      elt (gethash (cdr lst) ht-joins))))
		   nil
		   (nonrefinable-aux 
		    (cdr lst) (funcall join (list elt (car lst)))))))
	  ;; The above functions are the necessary predicates; the
	  ;; next function does the real work.
	  ;; lst is a join minimal set not joining above elt0. It
	  ;; is in reverse order. lst2 is all unused elements, so it
	  ;; is all elems which come after the (car lst) in the order
	  ;; of jis.
	  (find-min-jc (lst lst2)
	   (when lst2
		 (when (join-minimal-p lst (car lst2)) 
		       (let* ((lst3 (cons (car lst2) lst))
			      (join-lst3 (funcall join 
						  (list 
						   (car lst2) 
						   (gethash lst ht-joins)))))
			     (setf (gethash lst3 ht-joins) join-lst3)
			     (if (funcall le elt0 join-lst3)
				 (if (nonrefinable-p lst (car lst2))
				     (push lst3 join-covers))
				 (find-min-jc lst3 (cdr lst2)))))
		 (find-min-jc lst (cdr lst2)))) )
	 (find-min-jc nil jis) 
	 join-covers )))
	   
;;; The old join irredundant version just replaces join-minimal-p with
;;; irredundant-p.

;	  ;; This test if (cons elt lst) is irredundant.
;	  (irredundant-p (lst elt) 
;	   (if (funcall le elt (gethash lst ht-joins)) 
;	       nil 
;	       (irredundant-p-aux lst elt)))
;	  (irredundant-p-aux (lst elt) 
;	   (if (null lst) 
;	       t 
;	       (if (funcall le 
;		    (car lst) 
;		    (funcall join (list elt (gethash (cdr lst) ht-joins))))
;		   nil
;		   (irredundant-p-aux 
;		    (cdr lst) (funcall join (list elt (car lst)))))))
	   

;;; I have made make-j-list so that jis does not need to be just join
;;; irreducibles.

(defun make-j-list (elems lat)
  (mapcar #'(lambda (x)
      (list x (make-j x lat)))
    elems) )

;;; This uses the join covers and so is not polynomial, see the monograph
;;; for a discussion of the complexity of calculating the join covers.
;;; However, there is a polymonial time algorithm for finding the j(a)'s.
;;; We can calculate the a D b (dependency) relation in time O(nk^2) 
;;; (for all join irreducibles a and b). Then we can find the transitive
;;; closure (even if it has cycles) using Tarjar's algorithm, see
;;; Mehlhorn, p. 38 in time O(n^3).
;;; But since we we are working with these concepts we will usually want
;;; to use the join covers anyway, I won't change this now.


(defun make-j (elt lat
    &aux ans ans2)
  (setq ans (list elt))
  (loop
    (setq ans2 ans)
    (mapc #'(lambda (x)
        (setq
          ans (union ans (reduce
              #'(lambda (y z) (union y z :test #'equal))
              (funcall (lattice-join-covers lat) x) :initial-value nil) 
                     :test #'equal) ) )
      ans)
    (if (subsetp ans ans2 :test #'equal) (return ans)) )
  (cond
    ((member elt (lattice-ji-list lat) :test #'equal)
      ans )
    (t (delete elt ans :test #'equal)) ) )


(defun make-j-dual-list (elems lat)
  (mapcar #'(lambda (x)
      (list x (make-j-dual x lat)))
    elems) )

(defun make-j-dual (elt lat
    &aux ans ans2)
  (setq ans (list elt))
  (loop
    (setq ans2 ans)
    (mapc #'(lambda (x)
        (setq
          ans (union ans (reduce
              #'(lambda (y z) (union y z :test #'equal))
              (funcall (lattice-meet-covers lat) x) :initial-value nil) 
                     :test #'equal) ) )
      ans)
    (if (subsetp ans ans2 :test #'equal) (return ans)) )
  (cond
    ((member elt (lattice-mi-list lat) :test #'equal)
      ans )
    (t (delete elt ans :test #'equal)) ) )



;;; var-partition is the orbits of vars under the symmetry
;;; it is given as a list of blocks

(defvar *counter-example* nil)

;;; 8/90 I added compile to test. It made it a little fast on moderately
;;;      big examples.

(defun test= (v u vars &optional var-partition (lat *lat*)
    &aux elems seq test *lat*)
  (setq
    *lat* lat
    elems (lattice-elem-list lat)
    seq   (make-list (length vars) :initial-element (car elems))
    test  (compile nil (list 'lambda vars (list 'equal v u)))
    var-partition (make-var-order vars var-partition) )
  (loop
    ;; (if (subsetp '(a b c) seq :test #'equal) (break))
    (iF (not (apply test seq))
          (return-from test= (values nil seq)) )
    (if (null (next-ordered-seq seq elems var-partition))
        (return (values t nil)) ) ) )

(defun make-var-order (vars part
    &aux ans tmp)
  (setq part (mapcar #'(lambda (x) (sort x
          #'(lambda (y z) (>= (length (member y vars :test #'equal))
              (length (member z vars :test #'equal)))))) part) )
  (loop
    (if (null (cdr vars)) (return (nreverse ans)))
    (if (setq
        tmp (cdr (member (car vars)
            (find (car vars) part
                  :test #'(lambda (x y) (member x y :test #'equal)))
            :test #'equal)))
      (push (position (car tmp) vars :test #'equal) ans)
      (push nil ans) )
    (pop vars)))


;;; Even though this is called Horn test, it covers much more
;;; general sentences.

(defun test-horn (prec anti vars &optional var-partition (lat *lat*)
    &aux ELEMS SEQ TEST *lat*)
  (SETQ *lat* lat
        ELEMS (lattice-elem-list lat)
        SEQ   (MAKE-LIST (LENGTH VARS) :initial-element (CAR ELEMS))
        test (compile nil `(lambda ,vars (if ,prec (if ,anti t nil) t)))
        ;; TEST  (compile nil (LIST 'LAMBDA VARS (LIST 'EQUAL V U)))
        var-partition (make-var-order vars var-partition) )
  (LOOP
    ;; (if (subsetp '(a b c d) seq :test #'equal) (break))
    (if (NOT (APPLY TEST SEQ))
          (return-from test-horn (values NIL seq)) )
    (if (NULL (NEXT-ordered-SEQ SEQ ELEMS var-partition))
        (return (values t nil)) ) ) )

(defun find-all-counter-examples (prec anti vars &optional var-partition
                                       (lat *lat*)
    &aux ELEMS SEQ TEST counter-examples n *lat*)
  (setq n 0)
  (SETQ
    *lat* lat
    ELEMS (lattice-elem-list lat)
    SEQ   (MAKE-LIST (LENGTH VARS) :initial-element (CAR ELEMS))
    test `(lambda ,vars (if ,prec (if ,anti t nil) t))
    var-partition (make-var-order vars var-partition) )
  (LOOP
    (unless (APPLY TEST SEQ)
      (format t "~A    ~S~%" (incf n) seq)
      (push (copy-list seq) counter-examples) )
    (if (NULL (NEXT-ordered-SEQ SEQ ELEMS var-partition))
        (return counter-examples) ) ) )

;(DEFUN NEXT-SEQ (SEQ ELEMS
;   &aux TEMP)
;(cond
; ((NULL SEQ) NIL)
; ((NULL (CDR (SETQ TEMP (MEMBER (CAR SEQ) ELEMS :test #'equal))))
     ;;(CAR SEQ) must be in ELEMS
;     (cond
;       ((NEXT-SEQ (CDR SEQ) ELEMS)
;         (RPLACA SEQ (CAR ELEMS)) ))
;     (t NIL))
;  (t (RPLACA SEQ (SECOND TEMP)) ) ) )

;;; if seq is (a_0 ... a_{n-1}) then order-seq looks like
;;; (k_0 ... k_{n-2}), (it is one shorter than seq)
;;; where k_i > 0 or NIL. This then returns the next
;;; sequence satifsfying  a_i >= a_{i + k_i}

;;;; here
(defun next-ordered-seq (seq elems order-seq
    &aux temp)
  (cond
    ((NULL SEQ) NIL)
    ((NULL (CDR (SETQ TEMP (MEMBER (CAR SEQ) ELEMS :test #'equal))))
      ;; (CAR SEQ) must be in ELEMS
      (cond
        ((NEXT-ordered-SEQ (CDR SEQ) ELEMS (cdr order-seq))
          (if (car order-seq)
            (rplaca seq (nth (car order-seq) seq))
            (RPLACA SEQ (CAR ELEMS)) ) )
        (t nil)) )
    (t (RPLACA SEQ (SECOND TEMP)) ) ) )

;;;;;;;;;;;;;;;;;;;;;;  Power Sets   ;;;;;;;;;;;;;;;;;;;

;;; To get all  k  element subsets of an  n  element set  S,
;;; repeatedly use next-subset-aux starting with the tail of
;;; length  k  of set.

(defun next-subset (subset set
    &aux temp len1 len2)
  ;; no more of this length:
  (declare (fixnum len1 len2))
  (cond
    ((null (setq temp (next-subset-aux subset set)))
      (cond
        ((eql (setq len1 (length (the list subset))) 
              (setq len2 (length (the list set)))) nil)
        (t (copy-list (nthcdr (the fixnum (- len2 len1 1)) set))) ))
    (t temp)))

(defun next-subset-aux (subset set
    &aux TEMP)
 (declare (fixnum temp))
 (cond
  ((NULL subset) NIL)
  ((zerop (SETQ TEMP (position (CAR subset) set :test #'equal)))
   (cond
    ((NEXT-subset-aux (CDR subset) (cdr set))
      (RPLACA subset
        (nth (the fixnum (- (the fixnum (position 
                (second subset) set :test #'equal)) 1)) set) ))
    (t NIL)))
  (t (RPLACA subset (nth (the fixnum (- temp 1)) set)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Examples

;;; 1. Whitman failures

(defvar whit-prec
  ;'(and
    ;(< (length (member u elems :test #'equal))
       ;(length (member z elems :test #'equal)))
    ;(< (length (member y elems :test #'equal))
       ;(length (member x elems :test #'equal)))
    '(lssql (p x y) (s z u)) )

(defvar whit-anti '(or
    (lssql x (s z u))
    (lssql y (s z u))
    (lssql (p x y) z)
    (lssql (p x y) u) ))

(defvar whit-vars '(x y z u))

(defun whitman-failures ()
  (find-all-counter-examples whit-prec whit-anti whit-vars '((x y) (z u))) )

;;; Modularity

(defun modularp (&optional (lat *lat*))
  (test= '(p x (s y (p x z)))
         '(s (p x y) (p x z)) '(x y z) nil lat ) )

(defun distributivep (&optional (lat *lat*))
  (test= '(p x (s y z))
         '(s (p x y) (p x z)) '(x y z) '((y z)) lat ) )


;;; try with var-partition '((y z)) ; done 8/90


(defun distributivep2 (&optional (lat *lat*))
  (test= '(p (s x y) (s x z) (s y z))
         '(s (p x y) (p x z) (p y z)) '(x y z) '((x y z)) lat) )
