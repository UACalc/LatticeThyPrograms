; 1plus3.fp   

; 1 + 3, which is finite.

(in-package :user)

(setq oneplusthree
  (make-free-lattice
    '(a b c d )
    '(
      (a ())
      (d ())
      (b (c))
      (c (d)) ) ) )

(setq *lat* oneplusthree)

