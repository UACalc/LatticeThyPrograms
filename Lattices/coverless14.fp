;;; coverless14.fp

;;; The lattice generated by a, b, c with  ab = 0 and a + c = 1
;;; and bc, b+c, a+bc, a(b+c), ba^*, c+a_* with 
;;;
;;;	ba^* < c+a_*
;;; 
;;; Our J_0 set is {a, b, bc, (p a_* c), (p a^* c)}
;;; Its join closure give FL(2 + 1) with f(c) > f(b).
;;; Doubling f(c)/f(b) is obstructed not by a join cover but
;;; by the requirement ba^* < c+a_*. !!!!
;;; 

;;; old
;;; 
;;; In this lattice b is not cji and c is not cmi. The smallest w
;;; with w \leq b and \kappa(w) \geq c is 
;;; 
;;; 	w = b(a + bc)		\kappa(w) = c + a(b + c)
;;; 
;;; Note this refutes my conjecture that for p \nleq q in P, (p minimal,
;;; q maximal such) if there is a cji w \leq p and q \leq \kappa(w)
;;; then w is in P^\meet or the kappa of a cmi in P^\join.
;;; 
;;; 

(in-package :user)

(setq *lat*
  (let ((gens '(a b c bc b+c a^* a_* ba^* c+a_* 0 1))
        (uc '(
              (0 (a_* bc ))
	      (bc (ba^* c))
	      (b+c (1))
	      (a_* (a c+a_*))
	      (a^* (1))
	      (ba^* (b a^* c+a_*))
	      (c+a_* (b+c))
              (a (a^*))
              (b (b+c))
              (c (c+a_*))
              (1 ())

             ))
        (joins '(
                 (1 ((a c)  ))
                 (a^* ((a bc)  ))
                 (b+c ((b c)  ))
                 (c+a_* ((c a_*)  ))
              ))
        (meets '(
                 (0 ( (a b)  ))
                 (a_* ((a b+c)  ))
                 (bc ((b c)  ))
                 (ba^* ((b a^*)  ))
             )) )
    (make-fp-lattice gens uc joins meets t) )
)



