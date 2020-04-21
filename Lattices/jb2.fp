; jb2.fp

;;; this is one of jb's Q poset with FL(Q) infinite.
;;; b is not cji.

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
  (let ((gens '(0 1 a b c d f g))
        (uc '(
              (0 (a b c ))
              (d (1))
              (b (d f))
              (a (d))
              (f (1))
              (c (g))
              (g (f))
              (1 ())
           ))
        (meets '(
              (0 ((d g)))
              (b ((d f)))
             ))
        (joins '(
              (1 ((a c)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb2 *lat*)

