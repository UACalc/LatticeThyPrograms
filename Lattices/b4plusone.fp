; b4plusone.fp
;;; this is the lattice b4 with one additional element 0 < p < a.

;
;             o
;            /|\
;           / | \
;          /  |  \
;         o   o   o
;         |\ / \ /|
;         | x   x |		change this picture to b4
;         |/ \ / \|
;       a o   o   o
;          \  |  /
;         p o | /
;            \|/
;             o
;             a
;
;

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c d p ab ac ad bc bd cd abc abd acd bcd ))
        (uc '(
              (0 (p b c d))
              (p (a))
              (a (ab ac ad))
              (b (ab bc bd))
              (c (ac bc cd))
              (d (ad bd cd))
              (ab (abc abd))
              (ac (abc acd))
              (ad (abd acd))
              (bc (abc bcd))
              (bd (abd bcd))
              (cd (acd bcd))
              (abc (1))
              (abd (1))
              (acd (1))
              (bcd (1))
              (1 ())
           ))
        (meets '(
              (0 ((abc abd acd bcd) ))
              (a ((abc abd acd))) 
              (b ((abc abd bcd))) 
              (c ((abc acd bcd))) 
              (d ((abd acd bcd))) 
              (ab ((abc abd))) 
              (ac ((abc acd))) 
              (ad ((abd acd))) 
              (bc ((abc bcd))) 
              (bd ((abd bcd))) 
              (cd ((acd bcd))) 
             ))
        (joins '(
              (1 ((a b c d)))
              (ab ((a b)))
	      (ac ((a c)))
	      (ad ((a d)))
	      (bc ((b c)))
	      (bd ((b d)))
	      (cd ((c d)))
	      (abc ((a b c)))
	      (abd ((a b d)))
	      (acd ((a c d)))
	      (bcd ((b c d)))
             )))
    (make-fp-lattice gens uc joins meets nil ) ) )


