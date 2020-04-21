;;; w-failure.fp

;;; This is JB's example of a lattice which is semidistributive and 
;;; satisfies a < b in J(L) implies b \leq \kappa(a)^* and the dual
;;; property but is not bounded.

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d e f))
        (uc '(
	      (e (a b f))
	      (c (f))
	      (d (f))
	      (a ())
	      (b ())
	      (f ())
             ))
        (joins '(
	      (f ((c d)))
              ))
        (meets '(
	      (e ((a b)))
             )) )
    (make-fp-lattice gens uc joins meets ) ) )


(defun lower-bdd-p (&optional (lat *lat*))
  (let ((*lat* lat)
	(p-join (make-finite-lattice-from-lssql 
		 (cons '(s) (close-under (generators lat))) 
		 (lattice-lssql lat))) )
       (and 
	(= (length (lattice-ji-list p-join))
	   (length (lattice-ji-list (con p-join t))))
	(= (length (lattice-mi-list p-join))
	   (length (lattice-ji-list (con p-join)))) )))

(defun upper-bdd-p (&optional (lat *lat*))
  (let ((*lat* lat)
	(p-meet (make-finite-lattice-from-lssql 
		 (cons '(p) (close-under (generators lat) #'p)) 
		 (lattice-lssql lat))))
       (and 
	(= (length (lattice-ji-list p-meet))
	   (length (lattice-ji-list (con p-meet t))))
	(= (length (lattice-mi-list p-meet))
	   (length (lattice-ji-list (con p-meet)))) )))





