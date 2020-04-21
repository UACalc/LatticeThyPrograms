;;; slavik.fp	5/22/97

;;; This is the example on page 223 of the Birkhoff volume.

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
  (let ((gens '(0 a b c d e f g))
        (uc '(
              (0 (a b c))
              (d (f))
              (e ())
              (a (d))
              (b (d e))
              (c (e))
              (f (g))
              (g ())
             ))
        (joins '(
               (d ((a b)) )
               (e ((b c)) )
              ))
        (meets '(
              (0 ((g c) (e a)) )
              (b ((e g)) )
             )) )
    (make-fp-lattice gens uc joins meets t) ) )


