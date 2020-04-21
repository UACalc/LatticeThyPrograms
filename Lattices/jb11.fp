; jb11.fp


;;; This is infinite. e is ji but not cji.
;;;
;;;                  
;;;                              o 1
;;;                             /|         
;;;                            / o d        
;;;                           /  |\          
;;;                        f o c o \          
;;;                           \  |  \          
;;;                            \ o b o e        
;;;                             \|  /            
;;;                            a o /              
;;;                              |/                
;;;                            0 o                  


(in-package :user)


(setq *lat*
  (let ((gens '(a b c d e f 0 1))
        (uc '(
              (e (d))
              (b (c))
              (a (b f))
              (0 (a e))
              (c (d))
              (d (1))
              (f (1))
              (1 ())
           ))
        (meets '(
              (0 ((e c)))
             ))
        (joins '(
              (1 ((b f)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb11 *lat*)

