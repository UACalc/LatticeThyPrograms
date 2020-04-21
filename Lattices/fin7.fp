;;; fin7.fp	5/21/97

;;;

;;;        f o   o g
;;;          |\ /|
;;;        a o o o c   (b in the middle)
;;;          |/ \|
;;;        d o   o e
;;; 
;;;    b = d + e,  e = bc,  b = fg
;;; 
;;;  This is infinite a + b + c is not cmi.
;;;  If we add g = b + c, then the lattice is finite (15 elements and planar).
;;;  If we add f = a + b, then the lattice is finite (15 elements and planar).
;;; 




(in-package :user)

(defun jis (&optional (lat *lat*))
  (let* ((jis (remove-if-not #'(lambda (x) (ji-p x)) 
               (close-under (close-under (generators lat)) #'p)))) 
	jis ))

(defun mis (&optional (lat *lat*))
  (let* ((mis (remove-if-not #'(lambda (x) (mi-p x)) 
               (close-under (close-under (generators lat) #'p))))) 
	mis ))


(setq *lat*
  (let ((gens '(a b c d e f g))
        (uc '(
              (d (a b))
              (e (b c))
              (a (f))
              (b (f g))
              (c (g))
              (f ())
              (g ())
             ))
        (joins '(
              (b ((d e)) )
              (f ((a b)) )
              ))
        (meets '(
              (e ((b c)) )
              (b ((f g)) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


