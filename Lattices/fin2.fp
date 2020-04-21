;;; fin2.fp	5/20/97

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
              (f (a i j))
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


