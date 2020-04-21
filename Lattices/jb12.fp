; jb12.fp


;;; This is jb11.fp with b and c identified.
;;; It is still infinite.
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
  (let ((gens '(a b d e f 0 1))
        (uc '(
              (e (d))
              (b (d))
              (a (b f))
              (0 (a e))
              (d (1))
              (f (1))
              (1 ())
           ))
        (meets '(
              (0 ((e b)))
             ))
        (joins '(
              (1 ((b f)))
             )))
    (make-fp-lattice gens uc joins meets t ) )
  jb12 *lat*)

