;;; fin3.fp	5/20/97

;;; This is part of an investigation of when a(b+c) is cji.
;;; Under rather mild assumptions it isn't; but in this case it is.

(in-package :user)

(setq *lat*
  (let ((gens '(a b c d e f g h i j))
        (uc '(
              (d (i b))
              (e (j b))
              (a ())
              (b ())
              (c ())
              (f (a b i j))
              (g (c i))
              (h (c j))
              (i ())
              (j ())
             ))
        (joins '(
              (b ((d e)) )
              (i ((f g)) )
              (j ((f h)) )
              ))
        (meets '(
             )) )
    (make-fp-lattice gens uc joins meets t) ) )

;;; if w = a(b+c) then we get the following which shows that w is not cji
;;; because, eg., d \in J(w) and d is not cji.

;;; USER(93): (j (p 'a (s 'b 'c)))
;;; ((P A (S B C I J)) C D E H G F)
;;; USER(94): (pi-list *)
;;; 
;;; 0    a(b + c + i + j)
;;; 1    c
;;; 2    d
;;; 3    e
;;; 4    h
;;; 5    g
;;; 6    f
;;; USER(95): (s 'b 'f)
;;; B
;;; USER(96): (s 'f 'g)
;;; I
;;; USER(97): (s 'd 'e 'h 'f 'g)
;;; (S B I J)
;;; USER(98): (j (setf w (p 'a (s 'b 'c))))
;;; ((P A (S B C I J)) C D E H G F)
;;; USER(99): (mapcar #'kappa *)
;;; (NIL ((S A B I J)) NIL NIL ((S A B I)) ((S A B J)) NIL)

