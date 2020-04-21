; jb3a.fp

;;; This is one of jb's Q poset with FL(Q) infinite. But it grows slowly.
;;; b \meet e is not cji.

;;;                                 o 1
;;;                                /|\
;;;                               / | \
;;;                              /  |  \
;;;                           d o   o e o f
;;;                             |\ / \ /|
;;;                             | \   / |
;;;                             |/ \ / \|
;;;                           a o   o b o c
;;;                              \  |  /
;;;                               \ | /
;;;                                \|/
;;;                                 o 0

;;; JB's A part is 0,1,d,f,b while the B part is 0,1,a,c,e

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c d f e pprime h))
        (uc '(
              (0 (a pprime c ))
              (d (1))
              (e (h))
              (b (d f))
              (c (e f))
              (a (d e))
              (f (1))
	      (pprime (b))
	      (h (1))
              (1 ())
           ))
        (meets '(
              (b ((d f)))
              (0 ((d h) (f h)))
             ))
        (joins '(
              (e ((a c)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb3a *lat*)

