; chockt1.fp

;; not completed

(setq chockt1
  (make-fp-lattice
    '(a b c d e f g h i z u)
    '(
      (a (u))
      (b (e))
      (c (d))
      (u ())
      (d (u))
      (e (d))
      (f (a e))
      (g (f))
      (h (c g))
      (i (b g))
      (z (h i)) )
    '(
      (u ((a c) (a b)))
      (d ((c i)))
      (e ((b h)))
      (g ((h i))) )
    '(
      (f ((a d)))
      (i ((a b)))
      (h ((c e)))
      (z ((b c))) ) ) )

(setq *lat* chockt1 )
