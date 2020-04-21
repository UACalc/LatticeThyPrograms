(in-package 'user)
(load "bldnew")
(setq gens '(0 1 e s r p q u v ss rr pp qq uu vv)
      uc '(
           (0 (r s rr ss))
           (r (e v))
           (s (e u))
           (e (p q pp qq))
           (v (q))
           (u (p))
           (p (1))
           (q (1))
           (rr (e vv))
           (ss (e uu))
           (v (qq))
           (u (pp))
           (pp (1))
           (qq (1)) )
      joins '(
              (p ( (r u)))
              (q ( (s v)))
              (pp ( (rr uu)))
              (qq ( (ss vv)))
              (1 ( (u v) (uu vv))) ) )
(setq np32 (make-fp-lattice gens uc joins nil))
(add-cover-stuff np32)
(setq *lat* np32)
(save-lattice 'np32 "np32.sav")
(quit)


;
;                        o 1 = u+v
;                       / \
;                      /   \
;             v+s = q o     o p = u+r
;                    / \   / \
;                   /   \ /   \
;                  /     o e   \
;               v o      |      o u
;                  \     |     /
;                   \    o e* /
;                    \  / \  /
;                   r o     o s
;                      \   /
;                       \ /
;                        o
