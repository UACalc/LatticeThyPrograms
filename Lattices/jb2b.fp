; jb2b.fp

;;; this is jb2.fp with b removed.
;;; it is finite; but if 0 = d \meet f is replaced by 0 = d \meet g
;;; it is infinite.

;;;                                 o 1
;;;                                / \
;;;                               /   \
;;;                              /     \
;;;                           d o       o f
;;;                             |\     /|
;;;                             | \   / o g
;;;                             |  \ /  |
;;;                           a o   o b o c
;;;                              \  |  /
;;;                               \ | /
;;;                                \|/
;;;                                 o 0

;;; JB's A part is 0,1,d,f,g,b while the B part is 0,1,a,c

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a c d f g))
        (uc '(
              (0 (a c ))
              (d (1))
              (a (d))
              (f (1))
              (c (g))
              (g (f))
              (1 ())
           ))
        (meets '(
              (0 ((d f)))
             ))
        (joins '(
              (1 ((a c)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb2b *lat*)

