;;; conM3-3.fp

;;; This one has all the relation of the near D_2 we get in Con F_V(X).
;;; In this finitely presented lattice 
;;; 
;;;     \psi_1 \j (a \m b) \j (a \m c) \j (b \m c)  \cov  1
;;; 
;;; and this lattice modulo the maximal congruence separating these 2
;;; is Con(M_3) !!!!!!!!!!!!!!!!!!!!!!!
;;; 
;;; The finitely presented lattice given by the D_2 relation at the
;;; top of Con(M_3), where M_3 is the semilatttice.
;;;
;;; a b c are the atoms of M_3 and they here also represent the 
;;; corresponding coatom of Con(M_3). The coatom corresponding to 1
;;; we denote by psi1. We let
;;; aa by a \meet psi1, etc., and ab = a \mmet b, etc. 

(in-package :user)

(setq *lat*
  (let ((gens '(a b c psi1 aa bb cc zab zac zbc 1  ))
        (uc '(
              (zab (aa bb ))
              (zac (aa cc ))
              (zbc (bb cc ))
              (aa (a psi1))
              (bb (b psi1))
              (cc (c psi1))
              (a  (1))
              (b  (1))
              (c  (1))
              (psi1  (1))
              (1  ())

             ))
        (joins '(
                 (psi1 ((aa bb) (aa cc) (bb cc)))
                 (1 ((aa b) (aa c) (bb a) (bb c) (cc a) (cc b) ))
                 (aa ((zab zac)))
                 (bb ((zab zbc)))
                 (cc ((zac zbc)))
              ))
        (meets '(
                 (zab ((a b psi1) ))
                 (zac ((a c psi1) ))
                 (zbc ((b c psi1) ))
                 (aa ((a psi1) ))
                 (bb ((b psi1) ))
                 (cc ((c psi1) ))
             )) )
    (make-fp-lattice gens uc joins meets t) ) 
)



