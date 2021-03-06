;;; coverless12.fp

;;; The lattice generated by a, b, c with  bc = 0 and b + c = 1.
;;; 
;;; old:
;;; It appears that the only cji's in the lattice are ab and ac and
;;; the only cmi's are b and c.
;;; 
;;; 
;;; So we need to investigate when dean's conditions on the generators
;;; can be fixed by doubling (or some other construction).
;;; 

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d e f qab rcd 0 1))
        (uc '(
	      (0 (qab c d e f ))
	      (qab (a b rcd))
	      (a (1))
	      (b (1))
	      (e (1))
	      (f (1))
	      (c (rcd))
	      (d (rcd))
	      (rcd (1))
	      (1 ())

             ))
        (joins '(
		 (rcd ((c d)  ))
              ))
        (meets '(
		 (qab ( (a b)  ))
             )) )
    (make-fp-lattice gens uc joins meets t) ) 
)




