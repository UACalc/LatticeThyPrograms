; kappa.lisp 2/26/89

;;; This uses the same method as in Freese-Nation for finding
;;; kappa for nongenerators, using the fact that if w <= \Join K(w)
;;; only elements of  P  can be in  kappa(w). The key to see that
;;; the algorithm in F-N is correct is generalizing Thm 4.4.

;;;(in-package :user)

(defun kappa (elt &optional (lat *lat*))
  (if (lattice-p lat)
    (kappa-aux elt lat)
    (error "No current lattice.") ) )

; here: get correct ref to lat and fix ideal etc
(defun kappa-aux (elt lat
    ;; assumes elt is join irreducible
    &aux  kd  list j0 k dag m0 k-plus kappa-lw)
  (if (member elt (lattice-generators lat) :test #'equal)
    (return-from kappa-aux
      (second (assoc elt (lattice-kappa-list lat) :test #'equal)) ) )
  (setq kd (lattice-kappa-dual-list lat))
  (if
    (loop
      (if (null kd) (return list))
      (if (member elt (second (car kd)) :test #'equal)
        (push (caar kd) list))
      (pop kd) )
    (return-from kappa-aux list) )
  (setq
    j0 (remove elt (j elt lat) :test #'equal)
    dag (funcall (lattice-join lat) (ideal j0 elt lat))
    k  (remove-if #'(lambda (x) (lssql elt (join (list x dag) lat) lat) )
        (remove-if #'(lambda (y) (lssql elt y lat)) j0) ) )
  (if (lssql elt (setq kappa-lw (join k lat)) lat)
    (return-from kappa-aux nil))
  (loop
    (if (null j0) (return))
    (if (null (setq list (kappa-aux (pop j0) lat)))
      (return-from kappa-aux nil) )
    (setq m0 (union list m0 :test #'equal)) )
  ;(if (null m0) (return-from kappa-aux nil))
  (setq k-plus (funcall (lattice-meet lat) (filter m0 kappa-lw lat)))
  (list (join (list
      (funcall (lattice-join lat) (remove-if #'(lambda (x)
            (lssql elt (join (list x dag) lat) lat))
          (lattice-generators lat) ))
      (funcall (lattice-join lat)
        (mapcar #'(lambda (x) (meet (list k-plus x) lat) )
          (remove-if #'(lambda (y) (lssql elt y lat) ) m0) ) ) ) lat) ) )

;;; The hard case for j is when  w  is a meet, say
;;;
;;;        w = \Meet w_i \meet \Meet x_k
;;;
;;;          = \Meet\Join w_ij \meet \Meet x_k
;;;
;;; Note that {w_i1, ..., w_it} is a join cover of  w. It can be
;;; refined to a nonrefinable one,  T.  By theorem 6, each  w_ij
;;; which is not a generator is in this refinement. Any proper
;;; refinement of T fails to join to  w_i. If such a refinement S
;;; joined above  w  then by Dean's solution to the word problem,
;;; condition (6) of Thm 1 [Freese1989] would hold.  By the 
;;; canonical form there is no generator between  w  and  \Join S,
;;; since  \Join S <  w_i.  Thus (W) applies to  
;;;    
;;;		w = \Meet w_i <= \Join S
;;;
;;; It follows that if w_ij  is not in  P,  it must be in  J(w). 
;;; Similarly if  w_ij is
;;; a join irreducible element of  P,  it must be in P.  Claim if
;;; w_ij = y  is in  P,  and  y = \Join y_k,  where this is a
;;; nonrefinable join, then every proper, nonrefinable join cover
;;; T  of  y  is a nonrefinable join cover of some  y_k,  unless
;;; T = {y_k}.  Proof: T  is a join cover of each  y_k. If it is not
;;; proper, set  V_k = {y_k}, else  V_k  a refinement to a nonrefinable
;;; join cover of  y_k. If  V_k = {y_1, ..., y_n}  the claim is true,
;;; otherwise the union of the V_k's is a proper refinement of the  y's.
;;;
;;; This shows the procedure below is correct for joins and for the
;;; original  w  that for each  w_i  (which by assumptions is not
;;; in  P)  J(w_i) \subseteq J(w).  Thus we do not need to worry about
;;; join covers of  x_k  which also join above some  w_i (since
;;; J(w_i) \subseteq J(w)). Now any proper join cover of  w  must
;;; join above a  w_i  or an  x_k,  since  w  is in canonical form.
;;; Thus if we take the minimal join covers of all of the  x_k's and
;;; delete those which join above some  w_i,  we get the rest of  J(w).


(defun j (elt &optional (lat *lat*)
    &aux sub-elts ans non-gens non-gens2 jc jc2)
  (if (member elt (lattice-generators lat) :test #'equal)
    (return-from j
      (second (assoc elt (lattice-j-list lat) :test #'equal))) )
  (if (eql (car elt) 's)
    (return-from j
      (reduce #'(lambda (y z) (union y z :test #'equal))
        (mapcar #'(lambda (x) (j x lat)) (cdr elt)) :initial-value nil) ) )
  ;; now (car elt) = p
  (setq sub-elts (cdr elt))
  (loop
    (if (null sub-elts) (return))
    (cond
      ((member (car sub-elts) (lattice-generators lat) :test #'equal)
        (setq jc2 (funcall (lattice-join-covers lat) (pop sub-elts)))
        (loop
          (if (null jc2) (return))
          (cond
            ((member elt (car jc2) :test #'(lambda (x y) (lssql x y lat))))
            ((member (car jc2) jc :test #'(lambda (x y) (<< y x lat)) ))
            (t (setq jc (cons (car jc2)
		;; was delete-if
                (remove-if #'(lambda (x) (<< (car jc2) x lat)) jc) ))) )
          (pop jc2) ) )
      (t
        (push (car sub-elts) non-gens)
        (setq ans (union (j (pop sub-elts) lat) ans :test #'equal)) ) ) )
  (loop
    (if (null jc) (return (cons elt ans)))
    (setq
      non-gens2 non-gens
      jc2 (join (car jc) lat) )
    (loop
      (if (null non-gens2)
        (return (setq ans (union      ; changed 2/20/89
              (reduce #'(lambda (y z) (union y z :test #'equal))
                (mapcar #'(lambda (x)
                    (second (assoc x (lattice-j-list lat) :test #'equal)))
                  (car jc) ) :initial-value nil)
              ans :test #'equal)) ) )
      (if (lssql (pop non-gens2) jc2 lat) (return)) )
    (pop jc) ) )

;;;;;;;;;;;;;;;;        Upper Covers      ;;;;;;;;;;;;;;;;
;;
;; The idea is that if  w  is a join then by Thm 13 of Freese [1989]
;; the lower covers of  w  are determined from cji elements  u  which are
;; part of nonrefinable join representation of  w.  The proof of that
;; Thm shows how to calculate the lower covers given  u.  Since if  u
;; is not a generator things are like free lattices, ie.  (car (kappa u))
;; meet  w  is a lower cover of  w.  By Thm 6 these element are part of
;; the canonicl form of  w.
;;
;; So we do the following: for each  x  in  P,  and fror each  m  in
;; kappa(x) check if  w.m + x = w.  This is obvious necessary for  w
;; to be covered by  w.m.  For all such  m  in  kappa(x), take the
;; maximal elements of the form  w.m.


(defun lower-covers-fp (w &optional (lat *lat*))
  (cond
   ((ji-p w lat)
    (if (cji-p w lat)
        (list (meet (list w (car (kappa w lat))) lat))
        nil) )
   (t
    (let ((ans nil)
          (joinands (if (generator-p w lat) nil (cdr w)))
          (gens (remove-if-not #'(lambda (x) (ji-p x lat))
                               (ideal (generators lat) w lat))) )
         (loop
          ;(format t "~s~%" ans)
          (if (null gens) (return))
          (let* ((x (pop gens))
                 (kap (remove-if-not
                       #'(lambda (k)
                          (lssql w 
				 (join (list x (meet (list w k) lat)) lat)
				 lat))
                       (kappa x lat))))
                (setq ans (union
                 (mxl (mapcar #'(lambda (k) (meet (list w k) lat)) kap)
                      (lattice-lssql lat))
                 ans :test #'equal)) ) )
         (loop
          (if (null joinands) (return ans))
          (if (generator-p (car joinands) lat)
              nil
              (if (cji-p (car joinands) lat)
                  (pushnew
                   (meet (list (car (kappa (car joinands) lat)) w) lat)
                   ans)) )
          (pop joinands))))))

(defun upper-covers-fp (w &optional (lat *lat*))
  (cond
   ((mi-p w lat)
    (if (cmi-p w lat)
        (list (join (list w (car (kappa-dual w lat))) lat))
        nil) )
   (t
    (let ((ans nil)
          (meetands (if (generator-p w lat) nil (cdr w)))
          (gens (remove-if-not #'(lambda (x) (mi-p x lat))
                               (filter (generators lat) w lat))) )
         (loop
          ;(format t "~s~%" ans)
          (if (null gens) (return))
          (let* ((x (pop gens))
                 (kap (remove-if-not
                       #'(lambda (k)
                          (grtrql w 
				  (meet (list x (join (list w k) lat)) lat)
				  lat))
                       (kappa-dual x lat))))
                (setq ans (union
                 (mnl (mapcar #'(lambda (k) (join (list w k) lat)) kap)
                      (lattice-grtrql lat))
                 ans :test #'equal)) ) )
         (loop
          (if (null meetands) (return ans))
          (if (generator-p (car meetands) lat)
              nil
              (if (cmi-p (car meetands) lat)
                  (pushnew
                   (join (list (car (kappa-dual (car meetands) lat)) w) lat)
                   ans)) )
          (pop meetands))))))

(defmacro connected-component (w &optional (lat '*lat*) count)
  `(cc ,w ,lat ,count) )

(defun cc (w &optional (lat *lat*) count)
  (if (not count) (warn "Remember this could be infinite.~%~
	You may want to use the form: (cc <w> *lat* <count>).~%~
	Which only does <count> many passes."))
  (let* ((new (list w)) (old new) (n 0))
        (loop
         (multiple-value-bind (newold newnew) (cc-aux old new lat)
          (if (eql old newold)
              (return (values old nil))
              (setq old newold new newnew) )
          (incf n)
          (if (and count 
		   (> n count))
              (return (values old new)))
          (format t "Pass ~d~1,4T size: ~d~1,4T new: ~d~%"
                  n (length old) (length new) ) ) ) ) )

(defun cc-aux (old new &optional (lat *lat*))
  (let ((newnew nil))
       (loop
        (if (null new) (return (values old newnew)))
        (let ((upperlower (set-difference
                           (append (upper-covers-fp (car new) lat)
                                   (lower-covers-fp (pop new) lat))
                           old :test #'equal) ))
             (setq newnew (append upperlower newnew))
             (setq old (append upperlower old)) ))))
