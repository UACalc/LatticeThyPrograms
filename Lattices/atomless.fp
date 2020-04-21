; file atomless.fp


;      a   b   c
;      o   o   o
;       \  |  /
;        \ | /
;         \|/
;          o z

; with all three meets defined

(in-package :user)

(let* ((gens '(a b c z))
       (uc '(
           (a ())
           (b ())
           (c ())
	   (z (a b c))))
       (joins '())
       (meets '(
	      (z ((a b) (a c) (b c))))))
      (setq atomless (make-fp-lattice gens uc joins meets t)))

(setq *lat* atomless)


(let* ((gens '(a b c z u))
       (uc '(
           (u ())
           (a (u))
           (b (u))
           (c (u))
           (z (a b c))))
       (joins '() )
       (meets '(
              (z ((a b) (a c) (b c))))) )
      (setq atomless0 (make-fp-lattice gens uc joins meets t)))

(let* ((gens '(a b c z u))
       (uc '(
           (u ())
           (a (u))
           (b (u))
           (c (u))
           (z (a b c))))
       (joins '(
              (u ((a b)))))
       (meets '(
              (z ((a b) (a c) (b c))))) )
      (setq atomless1 (make-fp-lattice gens uc joins meets t)))

(let* ((gens '(a b c z u))
       (uc '(
           (u ())
           (a (u))
           (b (u))
           (c (u))
           (z (a b c))))
       (joins '(
              (u ((a b) (a c)   ))))
       (meets '(
              (z ((a b) (a c) (b c))))) )
      (setq atomless2 (make-fp-lattice gens uc joins meets t)))

(let* ((gens '(a b c z u))
       (uc '(
           (u ())
           (a (u))
           (b (u))
           (c (u))
           (z (a b c))))
       (joins '(
              (u ((a b) (a c) (b c)   ))))
       (meets '(
              (z ((a b) (a c) (b c))))) )
      (setq atomless3 (make-fp-lattice gens uc joins meets t)))






