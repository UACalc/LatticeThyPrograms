;;; nx2plustwo.fp


(in-package :user)
   

;;; This makes 2 \times n with all its joins and meets and two
;;; additional elements p and q with 
;;; 0 < p < (1,0),
;;; (0,n-1) < q < 1,
;;; p \join (0,n-1) = q
;;; q \meet (1,0)   = p
;;; 
;;; It is not hard to see that 2 \times n is a maximal sublattice.

(defun nx2plustwo (n)
  (let* ((gens '(p q))
         (uc '( (p (q (1 0))))) 
	 (uc (cons (list 'q (list (list 1 (- n 1)))) uc))
         (u '(1 0))
         (v (list 0 (- n 1)))
         (joins (list  (list 'q (list (list 'p v)))))
         (meets (list  (list 'p (list (list 'q u)))))
        )
        (dotimes (i n)
          (push (list 0 i) gens)
          (push (list 1 i) gens)
          (if (= i 0) 
            (progn
             (push (list '(0 0) (list 'p '(0 1))) uc)
             (push (list '(1 0) (list '(1 1))) uc) 
	     (push (list '(0 0) (list (list u v))) meets) )
            (if (= i (- n 1))
              (progn
               (push (list (list 0 i) (list 'q)) uc)
               (push (list (list 1 i) nil) uc) 
	       (push (list (list 1 i) (list (list u v))) joins) )
              (progn
               (push (list (list 0 i) (list (list 0 (+ i 1)) (list 1 i))) uc)
               (push (list (list 1 i) (list (list 1 (+ i 1)))) uc)
               (push (list (list 0 i) (list (list v (list 1 i)))) meets)
               (push (list (list 1 i) (list (list u (list 0 i)))) joins) ))))
	(setf *lat* (make-fp-lattice gens uc joins meets))
        ;uc
        ;joins
        ))
          


