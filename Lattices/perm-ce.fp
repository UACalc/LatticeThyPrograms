;;; perm-ce.fp

;;; This is JB's example of a lattice which is semidistributive and 
;;; satisfies a < b in J(L) implies b \leq \kappa(a)^* and the dual
;;; property but is not bounded.

(in-package :user)

(setq *lat*
  (let ((gens '(a0 a1 a2 b0 b1 b2 c0 c1 c2 x y z 
                a1+x a0+b2 a0+y b1+y b0+c2 b0+z c1+z c0+a2 c0+x))
        (uc '(
	      (x (a1+x c0+x))
	      (y (b1+y a0+y))
	      (z (c1+z b0+z))
	      (a0 (a1 a0+y a0+b2))
	      (a1 (a0+b2 a0+y a2 a1+x))
	      (a2 (a1+x c0+a2))
	      (b0 (b1 b0+z b0+c2))
	      (b1 (b0+c2 b0+z b2 b1+y))
	      (b2 (b1+y a0+b2))
	      (c0 (c1 c0+x c0+a2))
	      (c1 (c0+a2 c0+x c2 c1+z))
	      (c2 (c1+z b0+c2))
	      (a1+x ())
	      (a0+b2 ())
	      (a0+y ())
	      (b1+y ())
	      (b0+c2 ())
	      (b0+z ())
	      (c1+z ())
	      (c0+a2 ())
	      (c0+x ())
             ))
        (joins '(
	      (a1+x ((a1 x)))
	      (a0+b2 ((a0 b2)))
	      (a0+y ((a0 y)))
	      (b1+y ((b1 y)))
	      (b0+c2 ((b0 c2)))
	      (b0+z ((b0 z)))
	      (c1+z ((c1 z)))
	      (c0+a2 ((c0 a2)))
	      (c0+x ((c0 x)))
              ))
        (meets '(
             )) )
    (make-fp-lattice gens uc joins meets ) ) )

(setf perm-ce (make-finite-lattice-from-lssql 
	       (cons '(s) (closure (generators) #'s))
	       (lattice-lssql *lat*)))

(defun test-caspard (lat)
  (when (not (sd-meet-p lat))
	(format t "this lat fails SD-meet")
	(return-from test-caspard nil))
  (when (not (sd-join-p lat))
	(format t "this lat fails SD-join")
	(return-from test-caspard nil))
  (let* ((jis (lattice-ji-list lat))
	 (jis2 jis)
	 (mis (lattice-mi-list lat))
	 (mis2 mis)
	 k
	 (kappas (mapcar 
		  #'(lambda (x)
			    (car (set-difference
				  (filter mis (_* x lat) lat)
				  (filter mis x lat) :test #'equal)))
		  jis))
	 (dkappas (mapcar 
		  #'(lambda (x)
			    (car (set-difference
				  (ideal jis (^* x lat) lat)
				  (ideal jis x lat) :test #'equal)))
		  mis)))
	(dolist (x jis2 t)
		(setf k (join (list x (pop kappas)) lat))
		(unless (every #'(lambda (y) (lssql y k))
			       (filter jis x lat))
			(return-from-test-caspard nil)))
	(dolist (x mis2 t)
		(setf k (meet (list x (pop dkappas)) lat))
		(unless (every #'(lambda (y) (grtrql y k))
			       (ideal mis x lat))
			(return-from-test-caspard nil)) )))


