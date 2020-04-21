;;; coverless2.fp

;;; A modification of the fp lattice with no covers from my paper.
;;; This only makes (p a b) < (s c d) etc. but not (s a b c) = 1.
;;; This is a bounded homomorphic image of a free lattice so if it
;;; is not weakly atomic it is a counter-example to Kira's conjecture.
;;; uuu is the meet of the r's and vvv is the join of the q's. vvv < uuu
;;; and seemed a candidate for a coverless interval but w = a b^* c^*
;;; shows it is not. uu and vv are a better candidate and u and
;;; v is still better. I don't know about them. (That is, is there
;;; a cji below u with it kappa above v.) 
;;;
;;; Note kappa's are unique in this lattice.

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
		 ;;; (1 ((a b c) (a b d) (a c d) (b c d)))
              ))
        (meets '(
		 (qab ((a b)))
		 (qac ((a c))) 
		 (qad ((a d))) 
		 (qbc ((b c))) 
		 (qbd ((b d)))
		 (qcd ((c d)))
		 ;;; (0 ((a b c) (a b d) (a c d) (b c d)))
             )) )
    (make-fp-lattice gens uc joins meets t) ) 
  uuu (p 'rab 'rac 'rad 'rbc 'rbd 'rcd)
  vvv (s 'qab 'qac 'qad 'qbc 'qbd 'qcd)
  a^* (s 'a (p 'b 'c 'd))
  b^* (s 'b (p 'a 'c 'd))
  c^* (s 'c (p 'a 'b 'd))
  d^* (s 'd (p 'a 'b 'c))
  a_* (p 'a (s 'b 'c 'd))
  b_* (p 'b (s 'a 'c 'd))
  c_* (p 'c (s 'a 'b 'd))
  d_* (p 'd (s 'a 'b 'c))
  vv (s vvv (p a^* b^* c^*) 
          (p a^* b^* d^*) 
          (p a^* c^* d^*) 
          (p b^* c^* d^*) )
  uu (p uuu (s a_* b_* c_*) 
          (s a_* b_* d_*) 
          (s a_* c_* d_*) 
          (s b_* c_* d_*) )
  u (p (s a_* b_*) (s a_* c_*) (s a_* d_*) (s b_* c_*) (s b_* d_*) (s c_* d_*))
  v (s (p a^* b^*) (p a^* c^*) (p a^* d^*) (p b^* c^*) (p b^* d^*) (p c^* d^*))
)




