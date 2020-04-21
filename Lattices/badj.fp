; badj.fp   2/11/89

;;; This shows that my original definition of  j  in kappa.lsp
;;; is wrong. Namely, let  w = p.s1,  then  J(w) = {w, q1, r1}
;;; but the function gives  {w, q1, r1, q, r}.

;               o s = q + r    s1 = q1 + r1
;            . /|\
;          .  / | \
;      p o   o  oq or
;          s1|\/  /
;            |/\ /
;            o  o
;          q1    r1
;



(setq badj
  (make-fp-lattice
    '(p q r s s1 r1 q1)
    '(
      (s ())
      (p (s))
      (q (s))
      (r (s))
      (s1 (s))
      (q1 (q s1))
      (r1 (r s1)) )
    '(
      (s ((r q) ))
      (s1 ((r1 q1) )) )
    nil t))


(setq *lat* badj)
