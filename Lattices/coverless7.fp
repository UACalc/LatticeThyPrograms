;;; coverless7.fp

;;; A modification of the fp lattice with no covers from my paper.
;;; This only makes (p a b c) = 0 
;;; 
;;; This is interesting since it is not atomic and therefore not
;;; weakly atomic but it is not coverless (despite the title).
;;; In fact (p a b) etc are cji. These 6 are th cji's in P^
;;; they're closure is 2^6 and P is faithfully represented in there,
;;; as a poset. I had conjctured that this would imply weak atomicity,
;;; but see below.
;;; 
;;; Under the standard map a |-> ab + ac + ad and in the
;;; image a < ab + ac + ad + bc + bd + cd. If we try to double this
;;; interval, the result does not satisfy the definig relations.
;;; 
;;; So we need to investigate when dean's conditions on the generators
;;; can be fixed by doubling (or some other construction).
;;; 

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d 0  ))
        (uc '(
	      (0 (a b c d))

	      (a ())
	      (b ())
	      (c ())
	      (d ())

             ))
        (joins '(
              ))
        (meets '(
		 (0 ((a b c) (a b d) (a c d) ))
             )) )
    (make-fp-lattice gens uc joins meets t) ) 
  a^* (s 'a (p 'b 'c 'd))
  b^* (s 'b (p 'a 'c 'd))
  c^* (s 'c (p 'a 'b 'd))
  d^* (s 'd (p 'a 'b 'c))
  a_* (p 'a (s 'b 'c 'd))
  b_* (p 'b (s 'a 'c 'd))
  c_* (p 'c (s 'a 'b 'd))
  d_* (p 'd (s 'a 'b 'c))
  u (p (s a_* b_*) (s a_* c_*) (s a_* d_*) (s b_* c_*) (s b_* d_*) (s c_* d_*))
  v (s (p a^* b^*) (p a^* c^*) (p a^* d^*) (p b^* c^*) (p b^* d^*) (p c^* d^*))
)




