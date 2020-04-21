; 2plus2.fp   

; Rolf's lattice.

(in-package 'user)

(setq twoplustwo
  (make-free-lattice
    '(a b c d )
    '(
      (b (a))
      (d (c)) ) ) )

(setq *lat* twoplustwo)

