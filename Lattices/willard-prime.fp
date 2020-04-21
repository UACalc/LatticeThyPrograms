; willard-prime.fp	6/20/02

;;; This is the (nondual) of the fp lattice associated with 
;;; willard's nullary equation but with stronger relation making it
;;; semidistributive. It is willard's lattice modulo the smallest 
;;; congruence making it semidistributive.
;;;
;;; The relations are 
;;;         ac \leq b 
;;; and 
;;;         cd \leq a
;;;
;;; The extra relation is   
;;;
;;;       (a + c)bd \leq a and c
;;;
;;; This comes down to adding g = a + c and 0 = bdg.


(setq *lat*
  (let ((gens '(0 a b c d e f g))
        (uc '(
              (0 (e f))
	      (a (g))
	      (b ())
	      (c (g))
	      (d ())
              (e (a b c))
              (f (a c d))
              (g nil) ))
        (joins '(
               (g ((a c))) ))
        (meets '(
              (0 ((b d g)))
	      (e ((a b)))
	      (f ((c d))) )) )
    (make-fp-lattice gens uc joins meets t) ) )

