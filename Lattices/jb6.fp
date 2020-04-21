; jb6.fp

;;; this is one of jb's Q poset with FL(Q) infinite.
;;; the herringbone gives 
;;;
;;;       bc < b(a+bc) < b(c + b(a+bc)) < ...
;;;

;;;                               o 1
;;;                              / \ 
;;;                             /   o bb
;;;                            /   / \
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
  (let ((gens '(0 1 a b c bb d ))
        (uc '(
              (0 (b d ))
              (b (bb))
              (a (1))
              (c (bb))
              (bb (1))
              (d (a c))
              (1 ())
           ))
        (meets '(
              (d ((a bb)))
             ))
        (joins '(
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb6 *lat*)

