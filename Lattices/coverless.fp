;;; coverless.fp

;;; The fp lattice with no covers from my paper.

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d 0 1 rab rac rad rbc rbd rcd qab qac qad qbc qbd qcd))
        (uc '(
	      (0 (qab qac qad qbc qbd qcd))
	      (qab (a b rcd))
	      (qac (a c rbd))
	      (qad (a d rbc))
	      (qbc (b c rad))
	      (qbd (b d rac))
	      (qcd (c d rab))
	      (a (rab rac rad))
	      (b (rab rbc rbd))
	      (c (rac rbc rcd))
	      (d (rad rbd rcd))
	      (rab (1))
	      (rac (1))
	      (rad (1))
	      (rbc (1))
	      (rbd (1))
	      (rcd (1))
	      (1 ())
             ))
        (joins '(
		 (rab ((a b)))
		 (rac ((a c))) 
		 (rad ((a d))) 
		 (rbc ((b c))) 
		 (rbd ((b d)))
		 (rcd ((c d)))
		 (1 ((a b c) (a b d) (a c d) (b c d)))
              ))
        (meets '(
		 (qab ((a b)))
		 (qac ((a c))) 
		 (qad ((a d))) 
		 (qbc ((b c))) 
		 (qbd ((b d)))
		 (qcd ((c d)))
		 (0 ((a b c) (a b d) (a c d) (b c d)))
             )) )
    (make-fp-lattice gens uc joins meets ) ) )



