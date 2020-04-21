; fpstruct.lisp 3/1/89

;;; 12/15/93 Note that we could speed this up by using bitvectors
;;; to represent certain of the subsets of P, eg., the defined joins.
;;; Then to test if S, represented as a bitvector, has a subset
;;; join to p (as in ideal-gen), look at the defined joins to p
;;; has see if any is bit-leq than S. This takes time O(|P|) but
;;; testing subsets is quadratic.

;;; I discovered that kappa of a slim element can be exponentially
;;; long in \ell(w). So I am now calling cji-p-freelat to test cji
;;; since this is polynomial.

;;; The notation here is based on Freese "Finitely presented
;;; lattice: canonical forms and the covering relation," and
;;; my paper with Nation "Covers in free lattices." That the
;;; method for calculating J(w) is correct follows from carefully
;;; reading the proof Lemma 7 in the first paper. That the arguments
;;; for finding kappa(w), w not in P, are ok since Thm 4.4 of
;;; Freese-Nation cares over to FL(P).
;;;
;;; I commented out the join-covers stuff since make-join-ocvers
;;; wasn't working right. It is now fixed, but since it can be
;;; time consuming I'll make a separate function to install it.
;;;
;;; join-table and meet-table are poor name choices
;;;
;;; We need to check if join-table is correct wrt the order
;;; and that every join is proper.
;;;
;;; I replaced (atom ...)  with (generator-p ... lat) in 4 places.
;;;
;;; 11/90 I changed make-free-lattice to quote the unbound generators.

;(in-package "FINITELY-PRESENTED-LATTICE" :nicknames '("FP"))
;(export '(make-fp-lattice add-cover-stuff make-free-lattice << >> 
;   order-ideal order-filter lxp))
;(use-package "USER")
;(import '(*lat*) "USER")

;;;(in-package :user)

(defconstant *join-sign* 's)    (defconstant *meet-sign* 'p)
(defvar *alpha*)           (defvar *d-alpha*)
(defvar *alpha-lat*)

(defun make-fp-lattice (elems upper-covers join-table meet-table 
			      &optional covers-p)
  ;; elems, uc, joins and meets in P
  (let* ((temp (create-lattice)) 
	 (ji-list nil) 
	 (mi-list nil) 
	 ;; (*lat* temp) 	; deleting these 10/26/92
	 ;; (filters nil) 
	 )
    (setf (lattice-type temp) 'finitely-presented)
    (setf (lattice-zero temp) (list *join-sign*))
    (setf (lattice-one temp) (list *meet-sign*))
    (setf (lattice-filters temp)
      (make-filters (topological-sort upper-covers))) ; deleted elems for new
    (setf (lattice-lssql temp)			      ; version of make-filters
      #'(lambda (x y) (lssql-fp x y temp)))
    (setf (lattice-grtrql temp)
      #'(lambda (x y) (lssql-fp y x temp)))
    (setf (lattice-hash-table temp)
          (make-hash-table :test #'equal) )     ;; 11/90 trying equal
    (setf (lattice-generators temp) (remove-duplicates elems :test #'equal))
    (setf (lattice-elem-p temp) #'(lambda (w) (elem-p-fp w temp)))
    ;; put a check for the join and meet table
    (setf (lattice-join-table temp) join-table)
    (setf (lattice-meet-table temp) meet-table)
    (setf (lattice-ji-list temp)
      ;; could be a problem if join-table had a trivial join in it.
      (setq ji-list (remove-if #'(lambda (x)
            (second (assoc x join-table :test #'equal)))
          elems) ) )
    (setf (lattice-mi-list temp)
      ;; could be a problem if join-table had a trivial meet in it.
      (setq mi-list (remove-if #'(lambda (x)
            (second (assoc x meet-table :test #'equal)))
          elems) ) )
    (setf (lattice-join temp) #'(lambda (list) (join-fp list temp)))
    (setf (lattice-meet temp) #'(lambda (list) (meet-fp list temp))) 
    (setf (lattice-ji-p temp) 
	  #'(lambda (x) 
		    (cond 
		     ((generator-p x temp) 
		      (cond 
		       ((member x ji-list)) ) ) 
		     (t (eql (car x) 'p)) ) ) )
    (setf (lattice-mi-p temp) 
	  #'(lambda (x) 
		    (cond 
		     ((generator-p x temp) 
		      (cond 
		       ((member x mi-list)) ) ) 
		     (t (eql (car x) 's)) ) ) )
    (if covers-p (add-cover-stuff temp))
    temp) )

(defun add-cover-stuff (&optional (temp *lat*))
  (if (eq (lattice-type temp) 'finite-lattice)
   (add-cover-stuff-finite-lattice temp)
   (progn
    (setf (lattice-dag-list temp)
      (mapcar #'(lambda (x)
          (list x (funcall (lattice-join temp)
              (remove x (ideal (lattice-ji-list temp) x temp)
                      :test #'equal))) ) (lattice-ji-list temp)) )
    (setf (lattice-ddag-list temp)
      (mapcar #'(lambda (x)
          (list x (funcall (lattice-meet temp)
             (remove x (filter (lattice-mi-list temp) x temp)
                     :test #'equal))) ) (lattice-mi-list temp)) )
    (setf (lattice-kappa-dual-list temp)
      (make-kappa-dual-in-p temp) )
    (setf (lattice-kappa-list temp)
      (make-kappa-in-p temp) )
    (setf (lattice-join-covers-list temp)
;;; This requires 3 args. Should  the first be all gens of just the ji's
;;; Try just the ji's for now.
;;; Didn't work: now use all of P
      (make-join-covers-list 
       (lattice-generators temp) 
       (lattice-ji-list temp) 
       temp 
       #'(lambda (x) (cdr (assoc x (lattice-dag-list temp) :test #'equal)))))
    (setf (lattice-join-covers temp) ; now only for join irreds
      #'(lambda (x)
        (second (assoc x (lattice-join-covers-list temp) :test #'equal))))
    (setf (lattice-meet-covers-list temp)
      (make-meet-covers-list 
       (lattice-generators temp) 
       (lattice-mi-list temp) 
       temp
       #'(lambda (x) (cdr (assoc x (lattice-ddag-list temp) :test #'equal)))))
    (setf (lattice-meet-covers temp) ; now only for meet irreds
      #'(lambda (x)
        (second (assoc x (lattice-meet-covers-list temp) :test #'equal))))
    (setf (lattice-j-list temp)
      (make-j-list (lattice-generators temp) temp) )
    (setf (lattice-j temp)
      #'(lambda (x)
        (second (assoc x (lattice-j-list temp) :test #'equal))))
    (setf (lattice-j-dual-list temp)
      (make-j-dual-list (lattice-generators temp) temp) )
    (setf (lattice-j-dual temp)
      #'(lambda (x)
        (second (assoc x (lattice-j-dual-list temp) :test #'equal)))) 
    (setf (lattice-cji-p temp)
      #'(lambda (x) (and (ji-p x temp) (kappa x temp))) )
    (setf (lattice-cmi-p temp)
      #'(lambda (x) (and (mi-p x temp) (kappa-dual x temp))) )
    (setf (lattice-upper-covers temp)
      #'upper-covers-fp)
    (setf (lattice-lower-covers temp)
      #'lower-covers-fp)
    temp)))

;;; the free lattice over a poset

(defun make-free-lattice (gens &optional (uc nil supplied?))
  (mapc #'(lambda (x) (if (and (atom x) (not (boundp x))) 
			  (setf (symbol-value x) x))) 
	gens)
  (if (null uc) 	    ; added to make correct uc for new make-filters
      (setq uc (mapcar #'(lambda (x) (list x nil)) gens)))
  (let* ((temp (make-fp-lattice gens uc nil nil))
         ;; (zero '(s)) 
	 ;; (one '(p))
	 )
    ;; For free lattices, use a faster join and meet.
    (unless supplied?
     (setf (lattice-join temp) #'(lambda (list) (join-free list temp)))
     (setf (lattice-meet temp) #'(lambda (list) (meet-free list temp))))
    (setf (lattice-dag-list temp)
      (mapcar #'(lambda (x)
          (list x (funcall (lattice-join temp)
              (remove x (ideal (lattice-ji-list temp) x temp)
                      :test #'equal))) ) (lattice-ji-list temp)) )
    (setf (lattice-ddag-list temp)
      (mapcar #'(lambda (x)
          (list x (funcall (lattice-meet temp)
             (remove x (filter (lattice-mi-list temp) x temp)
                     :test #'equal))) ) (lattice-mi-list temp)) )
    (setf (lattice-join-covers-list temp)
      (mapcar #'(lambda (x) (list x nil)) gens) )
    (setf (lattice-meet-covers-list temp)
      (mapcar #'(lambda (x) (list x nil)) gens) )
    (setf (lattice-join-covers temp)
      #'(lambda (x)
        (second (assoc x (lattice-join-covers-list temp) :test #'equal))))
    (setf (lattice-meet-covers temp)
      #'(lambda (x)
        (second (assoc x (lattice-meet-covers-list temp) :test #'equal))))
    (setf (lattice-j-list temp)
      (mapcar #'(lambda (x) (list x (list x))) gens) )
    (setf (lattice-j-dual-list temp)
      (mapcar #'(lambda (x) (list x (list x))) gens) )
    (setf (lattice-j temp)
      #'(lambda (x)
        (second (assoc x (lattice-j-list temp) :test #'equal))))
    (setf (lattice-j-dual temp)
      #'(lambda (x)
        (second (assoc x (lattice-j-dual-list temp) :test #'equal))))
    (setf (lattice-kappa-list temp)
      (mapcar #'(lambda (x)
          (list x (list (funcall (lattice-join temp)
                (remove-if #'(lambda (y) (lssql x y temp)) gens))))) gens))
    (setf (lattice-kappa-dual-list temp)
      (mapcar #'(lambda (x)
          (list x (list (funcall (lattice-meet temp)
                (remove-if #'(lambda (y) (lssql y x temp)) gens))))) gens))
    (setf (lattice-ji-p temp)
      #'(lambda (x) (or (generator-p x temp) (eql (car x) 'p)))) 
    (setf (lattice-mi-p temp)
      #'(lambda (x) (or (generator-p x temp) (eql (car x) 's)))) 
    (setf (lattice-cji-p temp)
      #'(lambda (x) (and (ji-p x temp) (cji-p-freelat x temp))) )
    (setf (lattice-cmi-p temp)
      #'(lambda (x) (and (mi-p x temp) (cmi-p-freelat x temp))) )
    (setf (lattice-upper-covers temp)
      #'upper-covers-fp)
    (setf (lattice-lower-covers temp)
      #'lower-covers-fp)
    temp ) )

(defun elem-p-fp (w lat)
  (cond
   ((generator-p w lat) t)
   ((atom w) nil)
   ((or (eql (car w) *join-sign*) (eql (car w) *meet-sign*))
    (every #'(lambda (x) (elem-p-fp x lat)) (cdr w))) ) )

;;; Endomorphisms of free lattices

;(defun endo (w map &optional (lat *lat*))

(defun make-endo (map &optional (lat *lat*))
  (labels ((foo (x)
		(cond
                 ((generator-p x lat)
                  (let ((im (assoc x map :test #'equal)))
	               (if im 
			   (second im) 
			   x)))
                 ((eql (car x) *join-sign*)
		  (join (mapcar #'foo (cdr x)) lat))
		 ((eql (car x) *meet-sign*)
		  (meet (mapcar #'foo (cdr x)) lat))
		 (t (error "~s is not in the lattice." x)) ) ))
	  #'foo))




;;; make-join-covers-list is now in finstruct.lisp

(defun make-kappa-dual-in-p (lat
    &aux elems min-elems lat2 beta top)
  ;; lat is a fp lattice. This returns kappa-dual (when it exists)
  ;; in FL(P) of the elements of  P
  (if (null (setq elems (lattice-generators lat)))
      (return-from make-kappa-dual-in-p nil))
  (setq elems (close-under elems #'(lambda (&rest args)
                                     (funcall (lattice-meet lat) args))))
  (pushnew (setq top (funcall (lattice-join lat) elems)) elems
           :test #'equal)
  (setq lat2 (make-finite-lattice-from-grtrql elems (lattice-grtrql lat)))
  (setf (lattice-join-covers-list lat2)
	(make-join-covers-list elems (lattice-ji-list lat2) lat2
			       (lattice-lower-covers lat2)))
  (setf (lattice-join-covers lat2) ; now only for join irreds
        #'(lambda (x) (second (assoc x (lattice-join-covers-list lat2) 
				     :test #'equal))))
  (setf (lattice-j-list lat2) (make-j-list (lattice-ji-list lat2) lat2) )
  (setf (lattice-j lat2)
        #'(lambda (x) (second (assoc x (lattice-j-list lat2) 
				     :test #'equal))))
  (setq beta (make-beta
      (mapcar #'(lambda (elt) (list elt elt)) (lattice-ji-list lat2))
      lat
      lat2 ) )
  (mapcar #'(lambda (x)
      (setq min-elems
        (mnl (set-difference
            (ideal elems (second
                (assoc x (lattice-ddag-list lat) :test #'equal)) lat)
            (ideal elems x lat) :test #'equal) (lattice-grtrql lat) ) )
      (if (second (assoc (car min-elems) beta :test #'equal))
        (list x
          (mapcar #'(lambda (y)
              (second (assoc y beta :test #'equal))) min-elems ) )
        (list x nil) ) )
    (remove top (lattice-mi-list lat) :test #'equal) ) )

(defun make-kappa-in-p (lat
    &aux elems max-elems lat2 alpha bottom)
  ;; lat is a fp lattice. This returns kappa (when it exists)
  ;; in FL(P) of the elements of  P
  (if (null (setq elems (lattice-generators lat)))
      (return-from make-kappa-in-p nil))
  (setq elems (close-under elems #'(lambda (&rest args)
                                     (funcall (lattice-join lat) args))))
  (pushnew (setq bottom (funcall (lattice-meet lat) elems)) elems
           :test #'equal)
  (setq lat2 (make-finite-lattice-from-grtrql elems (lattice-grtrql lat)))
  (setf (lattice-meet-covers-list lat2)
	(make-meet-covers-list elems (lattice-mi-list lat2) lat2
			       (lattice-upper-covers lat2)))
  (setf (lattice-meet-covers lat2) ; now only for join irreds
        #'(lambda (x) (second (assoc x (lattice-meet-covers-list lat2) 
				     :test #'equal))))
  (setf (lattice-j-dual-list lat2) 
	(make-j-dual-list (lattice-mi-list lat2) lat2) )
  (setf (lattice-j-dual lat2)
        #'(lambda (x) (second (assoc x (lattice-j-dual-list lat2) 
				     :test #'equal))))
  (setq alpha (make-alpha
      (mapcar #'(lambda (elt) (list elt elt)) (lattice-mi-list lat2))
      lat
      lat2 ) )
  (mapcar #'(lambda (x)
      (setq max-elems
        (mxl (set-difference
            (filter elems (second
                (assoc x (lattice-dag-list lat) :test #'equal)) lat)
            (filter elems x lat) :test #'equal) (lattice-lssql lat) ) )
      (if (second (assoc (car max-elems) alpha :test #'equal))
        (list x
          (mapcar #'(lambda (y)
              (second (assoc y alpha :test #'equal))) max-elems ) )
        (list x nil) ) )
    (remove bottom (lattice-ji-list lat) :test #'equal) ) )

(defun make-beta (beta0 lat0 lat1 &aux jis-d d beta old-d ans jc j)
  ;; beta0 is a list of elems of the form (a b)
  ;; where  a  is a ji in lat1 and  b  is in lat0
  ;; the order of the list is the same as (lattice-ji-list lat1)
  ;; jis-d is the set difference of (lattice-ji-list lat1) and  d
  ;; which is initially empty
  (setq
    beta beta0
    jis-d (lattice-ji-list lat1) )
  (loop
    (setq
      old-d d
      d (append (remove-if
         #'(lambda (x)
            (setq j (funcall (lattice-j lat1) x))
            (not
             (loop
              (if (null j) (return t))  ; [do not remove]
              (setq jc (funcall (lattice-join-covers lat1) (car j)))
              (if (not
                  (loop
                    (if (null jc) (return t))
                    (if (not
                        (lssql
                          (second (assoc (car j) beta :test #'equal))
                          (funcall (lattice-join lat0) (mapcar
                              #'(lambda (y) (second
                                  (assoc y beta :test #'equal)))
                              (pop jc) )) lat0))
                      (return nil) ) ) )
                (return nil) )
              (pop j) ) ) )
          jis-d) d)
      jis-d (set-difference jis-d d :test #'equal) )
    (if (equal old-d d)
      (return (mapcar #'(lambda (x)
            (if (member (car x) d :test #'equal) x nil)) beta) ) )
    (setq beta (mapcar
        #'(lambda (x)
          (if (member (car x) d :test #'equal) x
            (progn
              (setq
                ans (second x)
                jc (funcall (lattice-join-covers lat1) (car x)))
              (list (car x)      ; might try rplacd
                (loop
                  (if (null jc) (return ans))
                  (if (subsetp (car jc) d :test #'equal)
                    (setq ans (funcall (lattice-meet lat0)
                        (list ans (funcall (lattice-join lat0)
                            (mapcar #'(lambda (y)
                                (second (assoc y beta :test #'equal)))
                              (car jc) )))) ) )
                  (pop jc) ) ) ) ) )
        beta)) ) )

(defun make-alpha (alpha0 lat0 lat1 &aux mis-d d alpha old-d ans mc j-dual)
  ;; alpha0 is a list of elems of the form (a b)
  ;; where  a  is a mi in lat1 and  b  is in lat0
  ;; the order of the list is the same as (lattice-mi-list lat1)
  ;; mis-d is the set difference of (lattice-ji-list lat1) and  d
  ;; which is initially empty
  (setq
    alpha alpha0
    mis-d (lattice-mi-list lat1) )
  (loop
    (setq
      old-d d
      d (append (remove-if
         #'(lambda (x)
            (setq j-dual (funcall (lattice-j-dual lat1) x))
            (not
             (loop
              (if (null j-dual) (return t))  ; [do not remove]
              (setq mc (funcall (lattice-meet-covers lat1) (car j-dual)))
              (if (not
                  (loop
                    (if (null mc) (return t))
                    (if (not
                        (grtrql
                          (second (assoc (car j-dual) alpha :test #'equal))
                          (funcall (lattice-meet lat0) (mapcar
                              #'(lambda (y) (second
                                  (assoc y alpha :test #'equal)))
                              (pop mc) )) lat0))
                      (return nil) ) ) )
                (return nil) )
              (pop j-dual) ) ) )
          mis-d) d)
      mis-d (set-difference mis-d d :test #'equal) )
    (when (equal old-d d) (setq *d-alpha* d *alpha* alpha *alpha-lat* lat1) 
      (return (mapcar #'(lambda (x)
            (if (member (car x) d :test #'equal) x nil)) alpha) ) )
    (setq alpha (mapcar
        #'(lambda (x)
          (if (member (car x) d :test #'equal) x
            (progn
              (setq
                ans (second x)
                mc (funcall (lattice-meet-covers lat1) (car x)))
              (list (car x)      ; might try rplacd
                (loop
                  (if (null mc) (return ans))
                  (if (subsetp (car mc) d :test #'equal)
                    (setq ans (funcall (lattice-join lat0)
                        (list ans (funcall (lattice-meet lat0)
                            (mapcar #'(lambda (y)
                                (second (assoc y alpha :test #'equal)))
                              (car mc) )))) ) )
                  (pop mc) ) ) ) ) )
        alpha)) ) )

; Based on:
; File FP.lsp (C) Ralph Freese (4/12/87) Last revised (4/12/87)

; We have modifies JOIN-AUX to reflect the new canonical form.  I.e.,
; an adequate term is now one in which if

;             w = w1 + ... + wn + x1 + ... xk

; where if  x <= w  then  x <= xj  for some  j.  The canonical form
; is the shortest adequate term representing  w.

; For the join of  L  I'm now setting  w = (CONS *JOIN-SIGN* L) and
; using this in RPL1, reasoning that if  wi <= wij <= w  and the latter
; because there is an  x  with  wij <= x <= w, then we will not need
; wij  anyway.  Better make sure this is true.  Right now it is too
; slow to use anyway.

;         *****  Finitely presented lattices  *******

;;; This may have to be rewritten as loop for efficiency.

(defun << (L M &optional (lat *lat*))         ;refinement
  (subsetp L M :test (lattice-lssql lat)) )

(defun >> (L M &optional (lat *lat*))         ;refinement
  (subsetp L M :test (lattice-grtrql lat)) )

;;; the order ideal in P generated by L
;;; changed 11/7/89 to avoid intersection since Steele does not 
;;; guarentee that it always takes from the first list.

(defun order-ideal (L P &optional (lat *lat*))
  (cond
    ((null L) L)
    ((generator-p L lat)
      (remove-if-not #'(lambda (x)
          (lssql X L lat)) P) )
    (t (remove-if-not #'(lambda (x) (member x L 
		:test #'(lambda (y z) (lssql y z lat)))) P) ) ) )
					

;the order filter in P generated by L

(defun order-filter (L P &optional (lat *lat*))
  (cond
    ((null L) L)
    ((generator-p L lat)
      (remove-if-not #'(LAMBDA (X)
          (grtrql X L lat)) P) )
    (t (remove-if-not #'(lambda (x) (member x L 
		:test #'(lambda (y z) (grtrql y z lat)))) P) ) ) )



; S is an antichain in P.  It returns the maximal elements of the ideal
; of P generated by S.  It modifies S.
; this one is about 1/3 faster than the old

(defun ideal-gen (S P &optional (lat *lat*) &aux P1)
  (setq P (remove-if #'(lambda (x)
              (member x S :test (lattice-lssql lat))) P)
        P1 P)
  (loop
    (if (null P) (return S))
    (if (some #'(lambda (x) (<< x S lat))
          (second (assoc (car P) (lattice-join-table LAT) :test #'equal)))
        (setq S (cons (car P)
                  (delete-if #'(lambda (x) (lssql x (car P) lat)) S) )
              P (delete-if #'(lambda (x) (lssql x (car P) lat)) P1)
              P1 P )
        (pop P) ) ) )


(defun filter-gen (S P &optional (lat *lat*) &aux P1)
  (setq P (remove-if #'(lambda (x)
              (member x S :test (lattice-grtrql lat))) P)
        P1 P)
  (loop
    (if (null P) (return S))
    (if (some #'(lambda (x) (>> x S lat))
          (second (assoc (car P) (lattice-meet-table LAT) :test #'equal)))
        (setq S (cons (car P)
                  (delete-if #'(lambda (x) (grtrql X (CAR P) lat)) S) )
              P (delete-if #'(lambda (x) (grtrql x (car P) lat)) P1)
              P1 P )
        (pop P) ) ) )
;;; here

; L is a list of elements in the lattice generated by P.  This
; function returns the maximal elements of the set of elements
; of P below the join of L.  It assumes each element is in
; normal form.

(defun ideal-below (L P lat)
  (ideal-gen (nmxl (remove-if #'(lambda (x)
          (not (some #'(lambda (y) (lssql x y lat)) L))) P)
                   (lattice-lssql lat)) P lat) )

(defun filter-above (L P lat)
  (filter-gen (nmnl (remove-if #'(lambda (x)
          (not (some #'(lambda (y) (grtrql x y lat)) L))) P)
                   (lattice-grtrql lat)) P lat) )


(defun join-fp (L &optional (lat *lat*) &aux L1 W)
  (cond
    ((null L)
      (lattice-zero lat))
    ((null (cdr L))
      (car L) )
    (t (setq
        L (copy-list L)
        w (cons *join-sign*
                (sort (nmxl
                       (rpl1-join
                        (nmxl (nconc
                               (setq L1 (ideal-below
                                         L
                                         (copy-list (lattice-generators lat))
                                         lat))
                               L)
                              (lattice-lssql lat))
                        (cons  *join-sign* (append L1 L))
                        lat)
                       (lattice-lssql lat)) #'(lambda (x y) (lxp x y lat)))) )
       (if (equal w (lattice-zero lat)) 
	   w
	   (if (null (cddr w)) (cadr w) w )))))


(defun meet-fp (L &optional (lat *lat*) &aux L1 W)
  (cond
    ((null L)
      (lattice-one lat))
    ((null (cdr L))
      (car L) )
    (t (setq
        L (copy-list L)
        w (cons *meet-sign*
                (sort (nmnl
                       (rpl1-meet
                        (nmnl (nconc
                               (setq L1 (filter-above
                                         L
                                         (copy-list (lattice-generators lat))
                                         lat))
                               L)
                              (lattice-grtrql lat))
                        (cons  *meet-sign* (append L1 L))
                        lat)
                       (lattice-grtrql lat)) #'(lambda (x y) (lxp x y lat)))) )
       (if (equal w (lattice-one lat)) 
	   w
	   (if (null (cddr w)) (cadr w) w )))))

;; The original version of this calls nmxl twice; both before and after
;; the call to rpl1-join. nmxl is quadradic and rpl1 is only linear
;; so I decided to eliminate the first call. Limited tests did not show
;; much difference. We may want to eleminate it from join-fp also.

(defun join-free (L &optional (lat *lat*) &aux W)
  (cond
    ((null L)
      (lattice-zero lat))
    ((null (cdr L))
      (car L) )
    (t (setq
        L (copy-list L)
        w (cons *join-sign*
                (sort (nmxl
                       (rpl1-join 
			L				; old code below
			;; (nmxl L (lattice-lssql lat))
                        (cons  *join-sign* L)
                        lat)
                       (lattice-lssql lat)) #'(lambda (x y) (lxp x y lat)))) )
       (if (equal w (lattice-zero lat)) 
	   w
	   (if (null (cddr w)) (cadr w) w )))))



(defun meet-free (L &optional (lat *lat*) &aux W)
  (cond
    ((null L)
      (lattice-one lat))
    ((null (cdr L))
      (car L) )
    (t (setq
        L (copy-list L)
        w (cons *meet-sign*
                (sort (nmnl
                       (rpl1-meet
			L
                        (cons  *meet-sign* L)
                        lat)
                       (lattice-grtrql lat)) #'(lambda (x y) (lxp x y lat)))) )
       (if (equal w (lattice-one lat)) 
	   w
	   (if (null (cddr w)) (cadr w) w )))))


(DEFUN WITNESSEXU (L U lat)
  (cond
   ((null l) nil)
   ((lssql (car l) u lat)
    (car l) )
   (t (witnessexu (cdr l) u lat)) ) )

(DEFUN WITNESSVEX (V L lat)
  (cond
   ((null l) nil)
   ((grtrql (car l) v lat)
    (car l) )
   (t (witnessvex v (cdr l) lat)) ) )


(DEFUN RPL1AUX-JOIN (U W lat &aux A)    ; note lat is not optional
  (cond
    ((generator-p U lat)
      (list U))
    ;;((NULL (CDR U))
      ;;(list U))
    ((EQ (CAR U) *JOIN-SIGN*)
      (RPL1-JOIN (CDR U) W lat))
    ((NULL (SETQ A (WITNESSEXU (CDR U) W lat)))
      (list U))
    (t (RPL1AUX-JOIN A W lat) )))

(DEFUN RPL1-JOIN (L W lat)
 (cond
  ((NULL L)
    NIL)
  (t (NCONC (RPL1AUX-JOIN (CAR L) W lat) (RPL1-JOIN (CDR L) W lat)) )))

(DEFUN RPL1AUX-meet (U W lat &aux A)    ; note lat is not optional
  (cond
    ((generator-p U lat)
      (list U))
    ;;((NULL (CDR U))
      ;;(list U))
    ((EQ (CAR U) *meet-SIGN*)
      (RPL1-meet (CDR U) W lat))
    ((NULL (SETQ A (WITNESSvEX w (CDR U) lat)))
      (list U))
    (t (RPL1AUX-meet A W lat) )))

(DEFUN RPL1-meet (L W lat)
 (cond
  ((NULL L)
    NIL)
  (t (NCONC (RPL1AUX-meet (CAR L) W lat) (RPL1-meet (CDR L) W lat)) )))

(DEFUN NSYM (W)
  (COND
    ((NULL W) 0)
    ((ATOM W) 1)
    (T (the fixnum
         (+ (the fixnum (NSYM (CAR W))) (the fixnum (NSYM (CDR W))))) ) ) )

(defparameter *lx-one* '(p))
(defparameter *lx-zero* '(s))
(defparameter *lx-ssign* 's)
(defparameter *lx-psign* 'p)

;;; 2/27/94 Fixed to cover the case generators are not atoms.

(DEFUN LXP (V U lat)
  (COND
    ((< (the fixnum (NSYM V)) (the fixnum (NSYM U))))
    ((< (the fixnum (NSYM U)) (the fixnum (NSYM V))) NIL)
    ((ATOM V)
      (cond
       ((numberp v)
	(if (numberp u) (< v u) t) )
       ((numberp u) nil)
       (t (STRING< V U)) ))
    ((generator-p v lat)
     (if (generator-p u lat)
	 (lx-list-p v u)
	 t ) )
    ((EQUAL V *LX-ONE*)
      (NOT (EQUAL U *LX-ONE*)))
    ((EQUAL V *LX-ZERO*)
      NIL)
    ((AND (EQUAL (CAR V) *LX-PSIGN*) (EQUAL (CAR U) *LX-SSIGN*)))
    ((AND (EQUAL (CAR V) *LX-SSIGN*) (EQUAL (CAR U) *LX-PSIGN*))
      NIL)
    (T (POP V)
      (POP U)
      (LOOP
        (IF (NULL V)
          (RETURN NIL))
        (IF (NOT (EQUAL (CAR V) (CAR U)))
          (RETURN (LXP (CAR V) (CAR U) lat)))
        (POP V)
        (POP U)) ) ) )

;;; This is a quick hack for generators which are not atoms. 
;;; When called (nsym v) = (nsym u).

(defun lx-list-p (v u)
  (cond
   ((atom v) 
    (cond
     ((numberp v)
      (if (numberp u) (< v u) t) ) 
     ((numberp u) nil)
     (t (string< v u)) ))
   (t 
    (loop
     (if (null v) (return nil))
     (if (not (equal (car v) (car u)))
	 (return (lx-list-p (car v) (car u))))
     (pop v)
     (pop u) ) ) ) )


;;; Connected component stuff, added 11/8/90

;(defun lower-covers (w &optional (lat *lat*))
;  (cond
;   ((mi-p w lat)
;    (if (cmi-p w lat) 
;	(list (meet (list w (car (kappa w lat))) lat))
;	nil) )






; This example closes at 22 elements with PSPS.  If we do not assume
; that (P A D) = Z then it closes slowly one element at a time.

;(SETQ *LAT* 'P1)

;(PUT 'P1 '*JOIN* 'JOIN-AUX)

;(PUT 'P1 '*MEET* 'MEET-AUX)

;(PUT 'P1 '*PRE-IN* 'PRE-IN)

;(PUT 'P1 '*ONE* '(P))

;(PUT 'P1 '*ZERO* '(S))

;(PUT 'P1 '*JOIN-SIGN* 'S)

;(PUT 'P1 '*MEET-SIGN* 'P)

;(PUT 'P1 '*GENERATORS* '(Z A B C D E))

;(PUT 'P1 '*FILTERS* '((Z (A B C D E)) (A (C E))
;        (C (E)) (B (D E)) (D (E)) ) )

;(PUT 'P1 '*JOINS* '((E ((C B) (A D)))))

;(PUT 'P1 '*MEETS* '((Z ((C B) (A D)))))

;(DEFUN CLOSEP (L)
;  (DUALIZE (CLOSES L)) )
