; n24plustwo.fp
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
  (let ((gens '(0 1 a0 a1 b0 b1 b2 b3 p q ))
        (uc '(
              (0 (p b0 ))
              (p (a0 q))
              (a0 (a1))
              (a1 (1))
              (b0 (b1))
              (b1 (b2))
              (b2 (b3))
              (b3 (q))
              (q (1))
              (1 ())
           ))
        (meets '(
              (0 ((a1 b3) ))
              (p ((a0 q))) 
             ))
        (joins '(
              (1 ((a0 b0)))
              (q ((b3 p)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  n24 *lat*)

;;;(setf n24 (generators *lat*))

;;; Starting with the generators of N24 (including p and q), closing 
;;; under join, applying f, closing under meet, etc., stabilizes to 
;;; the 30 element maximal sized minimal ext of N24.

(defun f (set)
  (let* ((q (s 'p 'b3))) 
	(append 
	 (interval set (p 'a1 (s 'p 'b0)) (p 'a1 q) *lat*)
	 (interval set (s 'p 'b0) (s 'b2 (p 'a1 q)) *lat*) 
	 (generators n24)) ))

;;; But doing the same process, with g in place of f, goes infinite.

(defun g (set)
  (let* ((q (s 'p 'b3))) 
	(append 
	 (interval set (p 'a1 (s 'p 'b0)) (s 'b2 (p 'a1 q)) *lat*)
	 (generators n24)) ))
