; File dean.fp   11/95

; this is the lattice in Dick dean's paper in the Monterey vol.
; the file solutions.lisp must be in the system.

(in-package :user)

(setq dean
  (make-fp-lattice
    '(a b c ab bc 0 1)
    '(
      (0 (ab bc))
      (ab (a b))
      (bc (b c))
      (a (1))
      (b (1))
      (c (1))
      (1 ()) )
    '(
      (b ((ab bc)))
      (1 ((a c))) )
    '(
      (ab ((a b)))
      (bc ((b c)))
      (0 ((a c))) ) ))


(setq dean2
  (make-fp-lattice
    '(a b c a1 a2 a21 a22 b1 b2 b21 b22 c1 c2 c21 c22 )
    '(
      (a ())
      (b ())
      (c ())
      (a1 (b21 c22 a))
      (b1 (c21 a22 b))
      (c1 (a21 b22 c))
      (a21 ())
      (a22 ())
      (b21 ())
      (b22 ())
      (c21 ())
      (c22 ())
      (a2 (a b1 c1))
      (b2 (b a1 c1))
      (c2 (c a1 b1)) )
    '(
      (a ((a1 a2)))
      (b ((b1 b2)))
      (c ((c1 c2))) )
    '(
      (a2 ((a21 a22)))
      (b2 ((b21 b22)))
      (c2 ((c21 c22))) )))


(setf dean-fl (make-free-lattice '(a1 a21 a22 b1 b21 b22 c1 c21 c22))
  *lat* dean-fl)

(setf 
  dean2-gens
  '(a1 a21 a22 b1 b21 b22 c1 c21 c22)
  dean2-rels 
  '(
    (a1 b21)
    (a1 c22)
    (b1 c21)
    (b1 a22)
    (c1 a21)
    (c1 b22)
    ((p b21 b22) a1)
    ((p c21 c22) a1)
    ((p a21 a22) b1)
    ((p c21 c22) b1)
    ((p b21 b22) c1)
    ((p a21 a22) c1)
    ((p b21 b22) a21)
    ((p c21 c22) a22)
    ((p c21 c22) b21)
    ((p a21 a22) b22)
    ((p a21 a22) c21)
    ((p b21 b22) c22)  ))
  
(setf sol0 (solution dean2-gens dean2-rels)
      sol1 (improve-solution dean2-gens dean2-rels sol0) )

(setf 
  aa1  (nth 0 sol1)
  aa21 (nth 1 sol1)
  aa22 (nth 2 sol1)
  bb1  (nth 3 sol1)
  bb21 (nth 4 sol1)
  bb22 (nth 5 sol1)
  cc1  (nth 6 sol1)
  cc21 (nth 7 sol1)
  cc22 (nth 8 sol1) )

(setf 
  aa2 (p aa21 aa22)
  bb2 (p bb21 bb22)
  cc2 (p cc21 cc22)
  aa  (s aa1 aa2)
  bb  (s bb1 bb2)
  cc  (s cc1 cc2) )

(setf ua (p aa bb21 cc22) ub (p bb cc21 aa22) uc (p cc aa21 bb22))

;;; try it without the a_1 > b_2 etc.

(setf 
  dean3-gens
  '(a1 a21 a22 b1 b21 b22 c1 c21 c22)
  dean3-rels 
  '(
    (a1 b21)
    (a1 c22)
    (b1 c21)
    (b1 a22)
    (c1 a21)
    (c1 b22)
    ;; ((p b21 b22) a1)
    ;; ((p c21 c22) a1)
    ;; ((p a21 a22) b1)
    ;; ((p c21 c22) b1)
    ;; ((p b21 b22) c1)
    ;; ((p a21 a22) c1)
    ((p b21 b22) a21)
    ((p c21 c22) a22)
    ((p c21 c22) b21)
    ((p a21 a22) b22)
    ((p a21 a22) c21)
    ((p b21 b22) c22)  ))


