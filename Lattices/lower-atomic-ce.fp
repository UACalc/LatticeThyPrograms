; lower-atomic-ce.fp	11/90

;;; This simple example shows that the following conjecture is wrong.
;;; If  w = w_1 \join ... \join w_n \join x_1 \join ... \join x_k
;;; canonically, then  w  is lower atomic iff each  w_i and each
;;; x_j is.  In this example take  w = p \join q.  The only
;;; nonrefinable join representation of  w  is
;;; p_1 \join p_2 \join q_2. (This can be derived easily from the
;;; fact that p_1, p_2, q_1, q_2 are the only ji generators.)
;;; Since p_1, p_2, and q_2 are all cji,  w  is lower atomic.
;;; However,  q  is not lower atomic. The only lower cover of  q
;;; is  p \meet q. The lower covers of w are
;;; p, p_1 \join q,  p_2 \join q
;;;
(setq *lat*
  (let ((gens '(p p1 p2 q q1 q2))
        (uc '(
              (p1 (p))
              (p2 (p))
              (q1 (p q))
              (q2 (q)) ))
        (joins '(
              (p ((p1 p2)))
              (q ((q1 q2)))) )
        (meets nil) )
       (make-fp-lattice gens uc joins meets t) ) )

;;;             p       q
;;;              o     o
;;;             /|\   / \
;;;            / | \ /   \
;;;           o  o  o     o
;;;          p1  p2 q1    q2
