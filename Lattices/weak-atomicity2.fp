;;; weak-atomicity2.fp

;;; The fp lattice generated by a, b, c, d with a + c = 1 and ac = 0. 
;;; and d < b.
;;; It still appears to be weakly atomic. In fact it is project so it is.
;;; So is the lattice of weak-atomicity.fp.
;;;
;;;
;;;		Old stuff
;;;
;;; Then P^\join
;;; is D_2, and P^\meet is 2^3 so FL(P) is a bounded lattice. 
;;; Is it weakly atomic?
;;; 
;;; This lattice is interesting. If we close under meets and then joins
;;; we get a 17 element lattice which is the same as Alan's PC (the 
;;; diagonally generated sublattice of the product of the 2 lattices in
;;; the first paragraph). But if we close first under joins and then
;;; under meets we get a 13 element distribitive lattice. What is interesing
;;; is that P is faithfully embedded in this lattice but b = ab + bc
;;; so it fails one of Dean's conditions on the generators. (There
;;; are no generators below ab or bc so the ideal they generate should 
;;; not contain b. 
;;; 
;;; This shows that an image of FL(P) which separtes P may not be good
;;; enough to be a start for Alan's doubling construction.
;;; 
;;; 			Weak Atomicity
;;; 
;;; Notice that if  f : FL(P) ->> L  is bounded and  L  is good enough
;;; to start the doubling then  FL(P)  is weakly atomic. I thought that
;;; if  FL(P)  was weakly atomic then we could take a congruence
;;; separating the prime intervals of P and take L to be the quotient
;;; lattice. Then L would be a bounded image and I thought then doubling 
;;; on L would yeild FL(P), showing the converse of the above. But now
;;; I'm not sure that such an L will be strong enough to start the doubling.
;;; 
;;; Question: is this lattice weakly atomic?
;;; 
;;; It probably is. But note no finite bounded image can be a doubling
;;; starting point (I think) because J(b) = {a, b, c} and L(b) is D_1
;;; and b has no lower cover. So if we suppose f : FL(P) ->> L is bounded
;;; and let J = \beta(J(L)) then b cannot be in J (else f is not upper
;;; bounded). But then in L f(b) will very likely be join reducible. 
;;; 
;;; 
;;; 
;;; 
;;; 

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d 0 1))
        (uc '(
	      (0 (a c d))
	      (d (b))
	      (a (1))
	      (b (1))
	      (c (1))
	      (1 ())
             ))
        (joins '(
		 (1 ((a c) ))
              ))
        (meets '(
		 (0 ((a c) ))
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


