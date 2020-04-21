
; File dean2.fp   11/95 
; the file solutions.lisp must be in the system.
; This tried a more symmectric way but it fails.
; If a', b', c' are the more complicated generators then they are supposed
; to generate a by a = a'b' + a'c'  but such a, b, and c won't generate
; a free lattice.

(in-package :user)

(setf dean-fl (make-free-lattice 
               '(x11 x12 x21 x22 y11 y12 y21 y22 z11 z12 z21 z22))
      *lat* dean-fl)

(setf 
  dean2-gens
  '(x11 x12 x21 x22 y11 y12 y21 y22 z11 z12 z21 z22)
  dean2-rels 
  '(
    ((p x11 x12) x22)     		; a inbetween
    ((p x11 x12) y11)
    ((p x11 x12) z21)

    ((p x21 x22) x12)
    ((p x21 x22) y11)
    ((p x21 x22) z21)

    ((p y11 y12) x12)
    ((p y11 y12) x22)
    ((p y11 y12) z21)

    ((p z21 z22) x12)
    ((p z21 z22) x22)
    ((p z21 z22) y11)

    ((p y11 y12) y22)			; b inbetween
    ((p y11 y12) z11)
    ((p y11 y12) x21)

    ((p y21 y22) y12)
    ((p y21 y22) z11)
    ((p y21 y22) x21)

    ((p z11 z12) y12)
    ((p z11 z12) y22)
    ((p z11 z12) x21)

    ((p x21 x22) y12)
    ((p x21 x22) y22)
    ((p x21 x22) z11)

    ((p z11 z12) z22)     		; c inbetween
    ((p z11 z12) x11)
    ((p z11 z12) y21)

    ((p z21 z22) z12)
    ((p z21 z22) x11)
    ((p z21 z22) y21)

    ((p x11 x12) z12)
    ((p x11 x12) z22)
    ((p x11 x12) y21)

    ((p y21 y22) z12)
    ((p y21 y22) z22)
    ((p y21 y22) x11)
    ))
  
(setf sol0 (solution dean2-gens dean2-rels)
      sol1 (improve-solution dean2-gens dean2-rels sol0) )

(setf 
  a11  	(nth 0 sol1)
  a12 	(nth 1 sol1)
  a21 	(nth 2 sol1)
  a22 	(nth 3 sol1)
  b11  	(nth 4 sol1)
  b12 	(nth 5 sol1)
  b21 	(nth 6 sol1)
  b22 	(nth 7 sol1)
  c11  	(nth 8 sol1)
  c12 	(nth 9 sol1)
  c21 	(nth 10 sol1)
  c22 	(nth 11 sol1) )

(setf 
  a1	(p a11 a12)
  a2	(p a21 a22)
  b1	(p b11 b12)
  b2	(p b21 b22)
  c1	(p c11 c12)
  c2	(p c21 c22)
  a	(s a1 a2)
  b	(s b1 b2)
  c	(s c1 c2) )


(setf sol2 (improve-solution dean2-gens dean2-rels sol1) )

(setf 
  a11  	(nth 0 sol2)
  a12 	(nth 1 sol2)
  a21 	(nth 2 sol2)
  a22 	(nth 3 sol2)
  b11  	(nth 4 sol2)
  b12 	(nth 5 sol2)
  b21 	(nth 6 sol2)
  b22 	(nth 7 sol2)
  c11  	(nth 8 sol2)
  c12 	(nth 9 sol2)
  c21 	(nth 10 sol2)
  c22 	(nth 11 sol2) )

(setf 
  a1	(p a11 a12)
  a2	(p a21 a22)
  b1	(p b11 b12)
  b2	(p b21 b22)
  c1	(p c11 c12)
  c2	(p c21 c22)
  a	(s a1 a2)
  b	(s b1 b2)
  c	(s c1 c2) )
