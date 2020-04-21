;;; lin-ext-new.lisp  	06/03/93

;;; This modifies lin-ext.lisp to be more of a breadth, rather than
;;; depth, first search, thus giving the "levels."

;;; Implimentation of "topological sort" as in Mehlhorn, v 2.
;;; "Data Structures and algorithms 2: Graph algorithms and 
;;; NP-completeness."

;;; Here's a comparison with the old lin-ext version:
;;; The first lattice is SPS(a b c) in fl3; it has 44 elements
;;; and e = 79 upper-covers relations. The second is PS(a b c d)
;;; in fl4. It is isomorphic to fd4 and has 166 elements and e = 452.

;;;			Lattice1	Lattice2
;;;	    n + e	123		618
;;;	t old ver	0.36		19.3
;;;	t new ver	0.35		 1.6
;;;	t new compiled 	0.07		 0.48

;;; This shows the dramatic improvement and shows that perhaps I
;;; should make more use of hash tables.

;;; This will eventually be called by linear-extension in the case
;;; of a finite lattice, ie., when (lattice-upper-covers-list lat)
;;; exits. 

;;;(in-package :user)

;;; This return a list of the elements, topologically sorted.
;;; The next one returns a list of pairs (a upper-covers-of-a),
;;; topologically sorted (on a of course)
;;; We should think of better names to distinguish the two..

(defun top-sort (lat-uc)
  (if (null lat-uc) 
      nil
      (let ((ans (poset-p lat-uc)))
	   (if ans 
	       ans
	       (error "This order contains a cycle!!!") ))))
   
;;; This return nil if the graph defined by upper-covers 
;;; is not an ordered set, ie., it is not acylic, otherwise 
;;; it returns a topological
;;; sort of the elements. upper-covers is a list of pairs of
;;; the form (a (b c ...)), where (b c ...) is a list of elements


(defun poset-p (upper-covers)
  (let* (
	 (sorted nil)
	 (n (length upper-covers))
	 (in-degrees (make-hash-table-safe :test #'equal :size n))
	 (uc-hash    (make-hash-table-safe :test #'equal :size n))
	 (zero-degrees nil)
	 (new-zero-degrees nil))
	(mapc #'(lambda (x) 
		 (setf (gethash (car x) in-degrees) 0)
		 (setf (gethash (car x) uc-hash) (second x)) ) 
	      upper-covers)
	(mapc #'(lambda (x) (mapc 
			     #'(lambda (y) (incf (gethash y in-degrees)))
			     (second x)))
	      upper-covers)
	(mapc #'(lambda (x) (if (= (gethash (car x) in-degrees) 0)
				(push (car x) zero-degrees)))
	      upper-covers)
	(loop
	 (if (null zero-degrees)
	     (if (null new-zero-degrees) 
		 (if (= n (length sorted)) 
		     (return (nreverse sorted)) 
		     (return nil))
		 ;; note it would be easy to make the level sets by
		 ;; pushing new-zero-degrees onto a list of levels
		 (setf zero-degrees new-zero-degrees
		       new-zero-degrees nil)) )
	 (push (car zero-degrees) sorted)
	 (mapc #'(lambda (x) 
		  (if (= 0 (decf (gethash x in-degrees)))
		      (push x new-zero-degrees)) )
	       (gethash (pop zero-degrees) uc-hash)))))

;;; This return the upper-covers-list topologically sorted (by the first
;;; which is the element.
(defun topological-sort (lat-uc)
  (let* (
         (sorted nil)
         (n (length lat-uc))
         (in-degrees (make-hash-table-safe :test #'equal :size n))
         (uc-hash    (make-hash-table-safe :test #'equal :size n))
         (zero-degrees nil)
         (new-zero-degrees nil))
        (mapc #'(lambda (x)
                 (setf (gethash (car x) in-degrees) 0)
                 (setf (gethash (car x) uc-hash) (second x)) )
              lat-uc)
        (mapc #'(lambda (x) (mapc
                             #'(lambda (y) 
				       ;; the if test allows this to be used
				       ;; with, eg., filters instead of lat-uc.
				       (if (not (equal (first x) y))
					   (incf (gethash y in-degrees))))
                             (second x)))
              lat-uc)
        (mapc #'(lambda (x) (if (= (gethash (car x) in-degrees) 0)
                                (push x zero-degrees)))
              lat-uc)
        (loop
	 (if (null zero-degrees)
	     (if (null new-zero-degrees) 
		 (if (= n (length sorted)) 
		     (return (nreverse sorted)) 
                     (error "This order contains a cycle!!!") )
		 ;; note it would be easy to make the level sets by
		 ;; pushing new-zero-degrees onto a list of levels
		 (setf zero-degrees new-zero-degrees
		       new-zero-degrees nil)) )
         (push (car zero-degrees) sorted)
         (mapc #'(lambda (x)
                  (if (= 0 (decf (gethash x in-degrees)))
                      (push (list x (gethash x uc-hash)) new-zero-degrees)) )
               (gethash (car (pop zero-degrees)) uc-hash)))))


(defun height-first-top-sort (upper-covers)
  (let* (
	 (sorted nil)
	 (n (length upper-covers))
	 (in-degrees (make-hash-table-safe :test #'equal :size n))
	 (uc-hash    (make-hash-table-safe :test #'equal :size n))
	 (zero-degrees nil))
	(mapc #'(lambda (x) 
		 (setf (gethash (car x) in-degrees) 0)
		 (setf (gethash (car x) uc-hash) (second x)) ) 
	      upper-covers)
	(mapc #'(lambda (x) (mapc 
			     #'(lambda (y) (incf (gethash y in-degrees)))
			     (second x)))
	      upper-covers)
	(mapc #'(lambda (x) (if (= (gethash (car x) in-degrees) 0)
				(push (car x) zero-degrees)))
	      upper-covers)
	(loop
	 (if (null zero-degrees)
		 (if (= n (length sorted)) 
		     (return (nreverse sorted)) 
		     (return nil)) )
	 (push (car zero-degrees) sorted)
	 (mapc #'(lambda (x) 
		  (if (= 0 (decf (gethash x in-degrees)))
		      (push x zero-degrees)) )
	       (gethash (pop zero-degrees) uc-hash)))))


