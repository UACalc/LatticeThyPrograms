;;; coverless3.fp

;;; Thisis all wong; go to coverless4.fp.

;;; A modification of the fp lattice with no covers from my paper.
;;; This only makes (p a b c) = 0 and (s a b c) = 1 and symmetrically.

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d 0 1 rabd rabd racd rbcd qabd qabd qacd qbcd))
        (uc '(
	      (0 (qabc qabd qacd qbcd))
	      (qabc (a b c))
	      (qabd (a b d))
	      (qacd (a c d))
	      (qbcd (b c d))

	      (a (rabc rabd racd))
	      (b (rabc rabd rbcd))
	      (c (rabc racd rbcd))
	      (d (rabd racd rbcd))

	      (rabc (1))
	      (rabd (1))
	      (racd (1))
	      (rbcd (1))
	      (1 ())
             ))
        (joins '(
		 (rabc ((a b c)))
		 (rabd ((a b d)))
		 (racd ((a c d)))
		 (rbcd ((b c d)))
		 (1 ((a b c d)))
              ))
        (meets '(
		 (qabc ((a b c)))
		 (qabd ((a b d)))
		 (qacd ((a c d)))
		 (qbcd ((b c d)))
		 (0 ((a b c d)))
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




