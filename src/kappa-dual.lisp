; kappa-dual.lisp 11/5/90
;
; a straight dualization

;;; This uses the same method as in Freese-Nation for finding
;;; kappa for nongenerators, using the fact that if w <= \Join K(w)
;;; only elements of  P  can be in  kappa(w). The key to see that
;;; the algorithm in F-N is correct is generalizing Thm 4.4.

;;;(in-package :user)

;; put a check that elt is ji here

(defun kappa-dual (elt &optional (lat *lat*))
  (if (lattice-p lat)
    (kappa-dual-aux elt lat)
    (error "No current lattice.") ) )

; here: get correct ref to lat and fix ideal etc
(defun kappa-dual-aux (elt lat
    ;; assumes elt is join irreducible
    &aux  kap  list j-dual0 m dag m-dual0 m-plus kappa-dual-lw)
  (if (member elt (lattice-generators lat) :test #'equal)
    (return-from kappa-dual-aux
      (second (assoc elt (lattice-kappa-dual-list lat) :test #'equal)) ) )
  (setq kap (lattice-kappa-list lat))
  (if
    (loop
      (if (null kap) (return list))
      (if (member elt (second (car kap)) :test #'equal)
        (push (caar kap) list))
      (pop kap) )
    (return-from kappa-dual-aux list) )
  (setq
    j-dual0 (remove elt (j-dual elt lat) :test #'equal)
    dag (funcall (lattice-meet lat) (filter j-dual0 elt lat))
    m  (remove-if #'(lambda (x) (grtrql elt (meet (list x dag) lat) lat) )
        (remove-if #'(lambda (y) (grtrql elt y lat)) j-dual0) ) )
  (if (grtrql elt (setq kappa-dual-lw (meet m lat)) lat)
    (return-from kappa-dual-aux nil))
  (loop
    (if (null j-dual0) (return))
    (if (null (setq list (kappa-dual-aux (pop j-dual0) lat)))
      (return-from kappa-dual-aux nil) )
    (setq m-dual0 (union list m-dual0 :test #'equal)) )
  ;(if (null m0) (return-from kappa-dual-aux nil))
  (setq m-plus (funcall (lattice-join lat) (ideal m-dual0 kappa-dual-lw lat)))
  (list (meet (list
      (funcall (lattice-meet lat) (remove-if #'(lambda (x)
            (grtrql elt (meet (list x dag) lat) lat))
          (lattice-generators lat) ))
      (funcall (lattice-meet lat)
        (mapcar #'(lambda (x) (join (list m-plus x) lat) )
          (remove-if #'(lambda (y) (grtrql elt y lat) ) m-dual0) ) ) ) lat) ) )

;;; The hard case for j is when  w  is a meet, say
;;;
;;;        w = \Meet w_i \meet \Meet x_k
;;;
;;;          = \Meet\Join w_ij \meet \Meet x_k
;;;
;;; Note that {w_i1, ..., w_it} is a join cover of  w. It can be
;;; refined to a nonrefinable one,  T.  By theorem 6, each  w_ij
;;; which is not a generator is in this refinement. Any proper
;;; refinement of T fails to join to  w_i. If such a refinement
;;; joined above  w  that would violate the canonical form. Hence
;;; an  w_ij  not in  P,  must be in  J(w). Similarly if  w_ij is
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


(defun j-dual (elt &optional (lat *lat*)
    &aux sub-elts ans non-gens non-gens2 mc mc2)
  (if (member elt (lattice-generators lat) :test #'equal)
    (return-from j-dual
      (second (assoc elt (lattice-j-dual-list lat) :test #'equal))) )
  (if (eql (car elt) 'p)
    (return-from j-dual
      (reduce #'(lambda (y z) (union y z :test #'equal))
        (mapcar #'(lambda (x) (j-dual x lat)) (cdr elt)) :initial-value nil) ) )
  ;; now (car elt) = p
  (setq sub-elts (cdr elt))
  (loop
    (if (null sub-elts) (return))
    (cond
      ((member (car sub-elts) (lattice-generators lat) :test #'equal)
        (setq mc2 (funcall (lattice-meet-covers lat) (pop sub-elts)))
        (loop
          (if (null mc2) (return))
          (cond
            ((member elt (car mc2) :test #'(lambda (x y) (grtrql x y lat))))
            ((member (car mc2) mc :test #'(lambda (x y) (>> y x lat)) ))
            (t (setq mc (cons (car mc2)
		;; was delete-if
                (remove-if #'(lambda (x) (>> (car mc2) x lat)) mc) ))) )
          (pop mc2) ) )
      (t
        (push (car sub-elts) non-gens)
        (setq ans (union (j-dual (pop sub-elts) lat) ans :test #'equal)) ) ) )
  (loop
    (if (null mc) (return (cons elt ans)))
    (setq
      non-gens2 non-gens
      mc2 (meet (car mc) lat) )
    (loop
      (if (null non-gens2)
        (return (setq ans (union      ; changed 2/20/89
              (reduce #'(lambda (y z) (union y z :test #'equal))
                (mapcar #'(lambda (x)
                    (second (assoc x (lattice-j-dual-list lat) :test #'equal)))
                  (car mc) ) :initial-value nil)
              ans :test #'equal)) ) )
      (if (grtrql (pop non-gens2) mc2 lat) (return)) )
    (pop mc) ) )



