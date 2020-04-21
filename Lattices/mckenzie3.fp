; mckenzie3.fp
;
; same lattice as mckenzie.fp

(setq mckenzie
  (make-fp-lattice
    '(x y z u v m)
    '(
      (m (x u v))
      (y (u))
      (z (v))
      (u nil)
      (x nil)
      (v nil) )
    nil
    '(
      (m ((x u) (x v) (u v))))
     t ) )

(setq *lat* mckenzie)

