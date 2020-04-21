; jb7.fp

;;; this is one of jb's Q poset with FL(Q) infinite.
;;; the herringbone gives 
;;;
;;;       bc \leq b(a+bc) \leq b(c + b(a+bc)) \leq ...
;;;
;;; These are all equal but if we reverse the roles of b and c
;;; they're distinct.
;;;
;;; The B' part of Q is {b, 0}.
;;;
;;;
;;;                               o 1
;;;                              / \ 
;;;                          aa o   o bb
;;;                            / \ / \
;;;                         a o c o   o b
;;;                            \ /   /  
;;;                           d o   /   
;;;                              \ /    
;;;                               o 0
;;;                                   
;;;                                    

;;; JB's A part is 0,1,a while the B part is 0,1,b,c

(in-package :user)


(setq *lat*
  (let ((gens '(0 1 a b c aa bb d ))
        (uc '(
              (0 (b d ))
              (b (bb))
              (a (aa))
              (aa (1))
              (c (aa bb))
              (bb (1))
              (d (a c))
              (1 ())
           ))
        (meets '(
              (d ((a bb)))
              (c ((aa bb)))
             ))
        (joins '(
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb7 *lat*)

(defun herringbone (a b c n)
  (if (= n 0) (p b c)
      (if (oddp n) 
	  (p b (s a (herringbone a b c (- n 1))))
	  (p b (s c (herringbone a b c (- n 1)))))))

(defun hb (a b c n)
  (let* ((ans (list (p b c))))
	(dotimes (k n)
		 (if (evenp k) 
		     (push (p b (s a (first ans))) ans)
		     (push (p b (s c (first ans))) ans)))
	ans ))



