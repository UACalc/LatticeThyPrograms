; file congrlat.lisp 5/89     Congruence lattices of lattices

;  This one puts partitions into a canonical form. All blocks must
;  have at least two elements and both the blocks and the members
;  of the blocks are sorted by general-lex-order.

;  partitions first

;  partitions are represented by blocks, eg. ((a d e) (c f))
;  one element blocks can be omitted

;  Ordering with blocks

;;;(in-package :user)

(defun general-lex-order (elt1 elt2)
 (cond
  ((atom elt1)
   (cond
    ((not (atom elt2)))
    ((numberp elt1)
      (cond
        ((numberp elt2) (< (the fixnum elt1) (the fixnum elt2)))
        (t t)))
    ((numberp elt2) nil)
    (t (string< elt1 elt2) )))
  ((atom elt2) nil)
  ((general-lex-order (car elt1) (car elt2)))
  ((not (equal (car elt1) (car elt2))) nil)
  (t (general-lex-order (cdr elt1) (cdr elt2))) ) )

(defun lss-partition (p1 p2)
  (loop
    (if (null p1) (return t))
    (if (not (member (car p1) p2
                     :test #'(lambda (x y) (subsetp x y :test #'equal))))
        (return nil))
    (pop p1) ) )

(defun grtr-partition (p1 p2)
  (loop
    (if (null p2) (return t))
    (if (not (member (car p2) p1
                     :test #'(lambda (x y) (subsetp x y :test #'equal))))
        (return nil))
    (pop p2) ) )

(defun intersectp (lst1 lst2 &key (test #'equal))
  (loop
    (if (null lst1) (return nil))
    (if (member (pop lst1) lst2 :test test)
        (return t) ) ) )

(defun join2-partition (p1 p2)
 (cond
  ((null p1) p2)
  (t (join2-partition (cdr p1) (join-par-aux (car p1) p2))) ) )


(defun join-par-aux (block par2
    &aux bigblock otherblocks)
  (setq bigblock (copy-list block))
  (loop
    (if (null par2)
      (return (merge 'list (list bigblock) (nreverse otherblocks)
        #'general-lex-order)))
    (if (intersectp (car par2) block :test #'equal)
      (setq bigblock (delete-duplicates
          (merge 'list (copy-list (pop par2)) bigblock
            #'general-lex-order) :test #'equal))
      (push (pop par2) otherblocks) ) ) )

(defun meet2-partition (p1 p2)
 (cond
  ((null p1) p1)
  (t (sort (append
        (meet-par-aux (car p1) p2)
        (meet2-partition (cdr p1) p2))
      #'general-lex-order) ) ) )

;;; Since intersection is not guarenteed to preserve the order,
;;; we can't use it.

(defun meet-par-aux (block par2
    &aux ans q1)
  (loop
    (if (null par2) (return (nreverse ans)))
    (if (cdr (setq q1
                   (remove-if-not
                     #'(lambda (x)
                               (member x (car par2) :test #'equal))
                               block) ))
      (push q1 ans) )
    (pop par2) ) )

(defun join-partition (lst)       ; was no-spread
  (if (null lst) nil
    (reduce #'join2-partition lst) ) )

;;; one is the greastest partition.

(defun meet-partition (lst &optional (one nil s))
  (if (null lst)
      (if s one
            (error
                "Unable to calculate the empty meet ~%~
                without knowing the greatest partition."))
      (reduce #'meet2-partition lst) ) )

(defun pre-in-partition (part)
 (cond
  ((atom part) 0)
  (t
    (setq part
      (nsubstitute #\[ #\( (nsubstitute #\] #\) (format nil "~S" part))))
    (setf (char part 0) #\{)
    (setf (char part (the fixnum (- (length (the simple-string part)) 1))) #\})
    part) ) )


;;;;;;;;     Congruence lattices of lattices     ;;;;;

;;; 7/26/90 added code for "lat" being an algebra.
;;;         Should change lat to alg or obj.

(defun con (lat &optional elems-p &aux congrlat)
  (unless (if (lattice-p lat) (lattice-elem-list lat) (algebra-elem-list lat))
	  (error "This ~a has nothing in its elem-list slot." 
		 (if (lattice-p lat) "lattice" "algebra")))
  (cond 
   ((algebra-p lat) 
    (setq congrlat (con-alg lat)))
   ((lattice-p lat) 
    (setq congrlat (con-lat lat elems-p)))
   (t (error "The first argument must be an algebra or lattice.")) )
  (when elems-p 
      (if (and 
	   (lattice-p lat)
	   (not (member (lattice-type lat) 
			'(finite-lattice finite-semilattice congruence))))
	  (setf (lattice-elem-list congrlat) 
		(cons nil (close-under (lattice-ji-list congrlat) 
				       #'(lambda (&rest args) 
						 (join-partition args))))) )
      (if (algebra-p lat) 
	  (setf (lattice-elem-list congrlat) 
            (if (= 1 (card lat))
              (list nil)
	      (linear-extension 
	       (cons nil (mapcar #'(lambda (x) (part-2-cong x lat))
			   (close-under (algebra-ji-congruences lat) 
			    #'(lambda (&rest args) (partition-join args))
			    :test #'equalp)))
		     congrlat)) ) ))
  congrlat)

(defun con-alg (alg)
  (if (algebra-con alg) 
      (algebra-con alg)
      (make-congrlat alg) ) )

;;; This is now in congrlat-lat.lisp.

;(defun con-lat (lat)
;  (if (lattice-con lat) 
;      (lattice-con lat)
;      (make-congrlat lat) ) )

(defun make-congrlat (obj)
  (let ((temp (create-lattice)))
    (setf (lattice-type temp) 'congruence)
    (setf (lattice-lssql temp) #'lss-partition)
    (setf (lattice-grtrql temp) #'grtr-partition)
    (cond ((algebra-p obj) 
	   (if (not (algebra-principal-congruences obj))
	       (setf (algebra-principal-congruences obj)
		     (principal-congruences obj))) 
	   (multiple-value-bind (congs congs-vector) (congs obj)
	     (setf (lattice-ji-list temp) 
		     (remove-if-not
                      #'(lambda (x) (ji-p-congs x congs #'join-partition 
					      #'lss-partition)) congs)
		   (algebra-ji-congruences obj)
		    (remove-if-not
                     #'(lambda (x) (ji-p-congs x congs-vector #'partition-join
					  #'partition-leq)) congs-vector))))
	  ((eql (lattice-type obj) 'finite-lattice) 
	   (setf (lattice-ji-list temp) (ji-con obj)) ) 
	  ((eql (lattice-type obj) 'congruence) 
	   (setf (lattice-ji-list temp) (ji-con obj)) )
	  ((eql (lattice-type obj) 'finite-semilattice) 
	   (setf (lattice-mi-list temp) (max-semilattice-con obj))
	   (setf (lattice-elem-list temp) 
		 (lin-ext (semilattice-con-aux obj) temp))
	   (setf (lattice-ji-list temp) 
		 (remove-if-not 
		  #'(lambda (x) 
		      (eql 1 (length (mxl (remove 
					   x
					   (ideal (lattice-elem-list temp)
						 x
						 temp))  ;; note test #eql
					  #'lss-partition))))
		  (lattice-elem-list temp))) ) 
	  (t (error
                "Con: the lattice type must be either finite-lattice ~% ~
                 or semilattice." )))
    (setf (lattice-zero temp) nil)
    (setf (lattice-one temp)
      (list (sort (copy-list (if (lattice-p obj)
                                 (lattice-elem-list obj)
                                 (algebra-elem-list obj)))
                  #'general-lex-order)))
    (setf (lattice-join temp) #'join-partition)
    (setf (lattice-meet temp)
          #'(lambda (lst) (meet-partition lst (lattice-one temp)))) 
    (setf (lattice-upper-covers temp)
     	      #'(lambda (x) 
    		 (if (lattice-elem-list temp) 
	             (mnl (remove x (filter (lattice-elem-list temp) x temp) 
			          :test #'equal)
		          #'grtr-partition))) )
    ;; this repeats calculation done above in the semilattice case
    (setf (lattice-lower-covers temp)
     	      #'(lambda (x) 
    		 (if (lattice-elem-list temp) 
	             (mxl (remove x (ideal (lattice-elem-list temp) x temp) 
			          :test #'equal)
		          #'lss-partition))) )
    (setf (lattice-pprint-elem temp) #'pre-in-partition)
    (if (lattice-p obj) (setf (lattice-con obj) temp))
    (if (algebra-p obj) (setf (algebra-con obj) temp))
    temp) )


;;; list is a "join dense" subset

(defun ji-p-congs (elem list join lq)
  (let* ((ideal (remove elem (remove-if #'(lambda (x) 
			(not (funcall lq x elem))) list) :test #'equal)))
	(or (null ideal)  (not (funcall lq elem (funcall join ideal))))))

; here This is probably not used

(defun semilattice-con (lat)
  (let ((temp (create-lattice)))
    (setf (lattice-type temp) 'congruence)
    (setf (lattice-elem-list temp) (semilattice-con-aux lat))
    (setf (lattice-lssql temp) 'lss-partition)
    (setf (lattice-grtrql temp) 'grtr-partition)
    (setf (lattice-join temp) 'join-partition)
    (setf (lattice-meet temp) 'meet-partition)
    (setf (lattice-pprint-elem temp) 'pre-in-partition)
    (setf (lattice-zero temp) nil)
    (setf (lattice-one temp)
      (list (sort (copy-list (lattice-elem-list lat))
          'general-lex-order)))
    temp) )

(defun cg (a b &optional (obj *lat*))  ; should be renaed to *obj* or *alg*
  (cond
    ((lattice-p obj) (cg-lat a b obj))
    ((algebra-p obj) (cg-alg a b obj))
    (t (error "The third argument must be a lattice or an algebra."))))


(defun cg-lat (a b lat
    &aux *join* *meet* *elements*)
  (declare (special *join* *meet* *elements*))
  (if (or (not (lattice-join lat)) (not (lattice-join lat)))
    (error "There is no lattice."))
  (setq *join* (lattice-join lat)
        *meet* (lattice-meet lat)
        *elements* (lattice-elem-list lat))
  (cg-aux (list (sort (list a b) #'general-lex-order))) )

(defun cg-aux (part
    &aux elems org org2 list)
  (declare (special *join* *meet* *elements*))
  (setq
    elems *elements*
    org2 part
    org part)
  (loop
    (if (null org2)
      (return (setq org2 part)) )
    ;;;(break)
    (loop
      (princ '+)
      (if (null elems) (return (setq elems *elements*)))
      ;;;(print part)
      (setq
        list (sort (delete-duplicates
            (mapcar #'(lambda (x)
                (funcall *join* (list (car elems) x))) (car org2))
            :test #'equal) #'general-lex-order) )
      (if (cdr list) (setq part (join-par-aux list part)))
      (pop elems) )
    (pop org2) )
  (if (eql (length (the list (car part))) (length (the list elems)))
    (return-from cg-aux part) )
  (loop
    (if (null org2)
      (return (setq org2 part)) )
    ;;;(break)
    (loop
      (princ '*)
      (if (null elems) (return (setq elems *elements*)))
      ;;;(print part)
      (setq
        list (sort (delete-duplicates
            (mapcar #'(lambda (x)
                (funcall *meet* (list (car elems) x))) (car org2))
            :test #'equal) #'general-lex-order) )
      (if (cdr list) (setq part (join-par-aux list part)))
      (pop elems) )
    (pop org2) )
  (if (eql (length (the list (car part))) (length (the list elems)))
    (return-from cg-aux part) )
  (if (equal part org)
    (return-from cg-aux part) )
  (cg-aux part) )

(defun ji-con (lat
    &aux list elems upper-covers )
  (setq
    elems (lattice-elem-list lat))
  (loop
    (if (null elems) (return list))
    (if (eql (length
          (the list (setq
            upper-covers
            (funcall (lattice-upper-covers lat) (car elems))))) 1)
      (pushnew (cg (pop elems) (car upper-covers) lat) list
        :test #'equal)
      (pop elems) ) ) )


;;;;;;;;     Congruence lattices of meet semilattices     ;;;;;


;(defun meet-cg (a b lat
    ;*join* *meet* *elements*)
  ;(if (not lat) (setq lat *lat*))
  ;((null lat) (writeline "No *lat*!!!") nil)
  ;(if (setq *meet* (lattice-meet lat))
    ;nil
    ;(progn
      ;(princ "Cg: No meet defined for " t)
      ;(print lat t)
      ;nil) )
  ;(setq *elements* (lattice-elem-list lat))
  ;(meet-cg-aux (list (sort (list a b) 'general-lex-order))) )


;(defun meet-cg-aux (part
    ;elems org org2 list)
  ;(setq
    ;elems *elements*
    ;org2 part
    ;org part)
  ;(loop
    ;((null org2) (setq org2 part))
    ;(loop
      ;(princ '*)
      ;((null elems)
        ;(setq elems *elements*))
      ;;;(print (car elems))
      ;;;(print part)
      ;(setq
        ;list (sort (delete-duplicates
            ;(mapcar '(lambda (x)                ; list added 1/89
                ;(funcall *meet* (list (car elems) x))) (car org2))
            ;equal) 'general-lex-order) )
      ;(if (cdr list) (setq part (join-par-aux list part)))
      ;(pop elems) )
    ;(pop org2) )
  ;((eql (length (car part)) (length elems)) part)
  ;((equal part org) part)
  ;(meet-cg-aux part) )

(defun max-semilattice-con (lat
    &aux tmp elems elems-0)
  (setq
    elems (lattice-elem-list lat)
    elems-0 (remove (funcall (lattice-meet lat) ; was apply
        ;(copy-list elems)) elems 'equal))
        elems) elems :test #'equal))
  (mapcar #'(lambda (x)
      (if (cdr (setq tmp
            (copy-list (second (assoc x (lattice-filters lat)
                  :test #'equal)))))
        (sort (list
            (setq tmp (sort tmp #'general-lex-order))
            (sort (set-difference elems tmp :test #'equal)
              #'general-lex-order))
          #'general-lex-order)
        (list (sort (remove (car tmp) elems :test #'equal)
            #'general-lex-order)) ))
    elems-0) )

(defun semilattice-con-aux (lat)
  ;; why didn't I add the 1 last??
  (close-under (cons
      (list (sort (copy-list (lattice-elem-list lat)) #'general-lex-order))
      (max-semilattice-con lat))
    #'(lambda (&rest args) (meet-partition args)) ) )

;;; This should is now in general-fns.lisp

;(defun distributive-p (&optional (lat *lat*))
  ;(case (lattice-type lat)
	;(congruence (distributive-lat-p (lattice-ji-list lat) lat))
	;(t (error "There is only code for testing distributivity of ~
		  ;congruence lattices."))))
;
;;;; This test if each join irreducible is join prime.
;
;(defun distributive-lat-p (jis lat &aux (jis2 jis))
  ;(loop
   ;(if (null jis2) (return t))
   ;(if (lss-partition 
	;(car jis2) 
	;(join-partition (set-difference jis (filter jis (car jis2) lat))))
       ;(return (values nil (car jis2))) )
   ;(pop jis2) ) )

(defmacro mod-partition (elt par)
  `(let ((block (find ,elt ,par 
		      :test 
			#'(lambda (x y) (member x y :test #'equal)))))
	(if (null block) (list ,elt) block)))

(defun permute-p (par1 par2)
  (if (or (lss-partition par1 par2) (lss-partition par2 par1))
      t 
      (let ((par3 (join-partition (list par1 par2))))
	   (loop
	    (if (null par3) (return t))
	    (let* ((block (pop par3))
		   (block2 block))
		 (loop
		  (if (null block) (return))
		  (let* ((block3 block2) 
			 (elt1-mod-par1 (mod-partition (pop block) par1)))
		       (loop
			(if (null block3) (return))
			(if (not (intersectp elt1-mod-par1
					     (mod-partition (pop block3)
							    par2)))
			    (return-from permute-p nil))))))))))

(defun permuting-lat (&optional (lat *lat*))
  (if (or (not (lattice-p lat)) (not (eql (lattice-type lat) 'congruence))) 
      (error "permuting-con: the argument must be a congruence lattice."))
  (let ((jis (lattice-ji-list lat)))
       (if (not jis) (error "There are no join-irreducibles."))
       (loop
	(if (null (cdr jis)) (return t))
	(let ((jis2 (cdr jis)))
	     (loop
	      (if (null jis2) (return))
	      (if (not (permute-p (car jis) (car jis2)))
		  (return-from permuting-lat
			       (values nil (car jis) (car jis2))))
	      (pop jis2))) 
	(pop jis))))



(defun permuting-con-p (alg)
  (permuting-lat (con alg)))

(defmacro perm-con-p (alg)
  `(permuting-con-p ,alg) )

(defun distributive-con-p (alg)
  (distributive-p (con alg)))

(defmacro dist-con-p (alg)
  `(distributive-con-p ,alg))


