;;; general-fns.lisp  8/15/90

;;; functions to be used at the top level

;;;(in-package :user)

(defun univ (obj)
  (universe obj))

;(defmacro univ (obj)
;  `(universe ,obj) )

(defun universe (obj)
  (cond
   ((algebra-p obj)
    (algebra-elem-list obj) )
   ((lattice-p obj)
    (lattice-elem-list obj) )
   (t (error "The argument to universe must be a lattice or algebra.")) ) )

(defun cardinality (obj)
  (length (universe obj)) )

(defun card (obj)
  (cardinality obj) )

;(defmacro cardinality (obj)
;  `(length (universe ,obj)) )
;
;(defmacro card (obj)
;  `(cardinality ,obj) )

(defun upper-covers (elt &optional (lat *lat*))
  (if (not (lattice-upper-covers lat))
      (if (eql (lattice-type lat) 'finitely-presented)
       (progn
        (warn "Adding cover stuff to your lattice; this could take awhile.")
        (add-cover-stuff lat))
       (error
       "The lattice argument ~s does not have a upper-covers routine." lat)))
  (funcall (lattice-upper-covers lat) elt) )

(defun ^* (x &optional (lat *lat*))
  (let* ((uc (upper-covers x lat)))
	(if (= (length uc) 1)
	    (car uc))))

(defun lower-covers (elt &optional (lat *lat*))
  (if (not (lattice-lower-covers lat))
      (if (eql (lattice-type lat) 'finitely-presented)
       (progn
        (warn "Adding cover stuff to your lattice; this could take awhile.")
        (add-cover-stuff lat))
       (error
       "The lattice argument ~s does not have a lower-covers routine." lat)))
  (funcall (lattice-lower-covers lat) elt) )

(defun _* (x &optional (lat *lat*))
  (let* ((lc (lower-covers x lat)))
	(if (= (length lc) 1)
	    (car lc))))

(defun generator-p (elt &optional (lat *lat*))
  (member elt (lattice-generators lat) :test #'equal) )

;; try omitting this 7/26/2015
;(defun elem-p (elt &optional (lat *lat*))
;  (lattice-elem-p elt lat))

(defun generators (&optional (lat *lat*))
  (lattice-generators lat))

;(defmacro generator-p (elt &optional (lat '*lat*))
;  `(member ,elt (lattice-generators ,lat) :test #'equal) )
;
;(defmacro elem-p (elt &optional (lat '*lat*))
;  `(lattice-elem-p ,elt ,lat))
;
;(defmacro generators (&optional (lat '*lat*))
;  `(lattice-generators ,lat))

(defun between (x a b &optional (lat *lat*))
  (and (lssql a x lat) (lssql x b lat)))

(defun distributive-p (&optional (lat *lat*))
  (case (lattice-type lat)
        (congruence (distributive-lat-p (lattice-ji-list lat) lat))
        (finite-lattice (distributive-fin-lat-p (lattice-ji-list lat) lat))
        (t (error "There is only code for testing distributivity of ~
                  congruence and finite lattices."))))

;;; The semidistributive laws. We need to add code for congruences lattices
;;; finding the mi is potentiall exponential.

(defun sd-meet-p (&optional (lat *lat*))
  (case (lattice-type lat)
        ;;(congruence (sd-meet-lat-p (lattice-ji-list lat) lat))
        (finite-lattice (sd-meet-fin-lat-p (lattice-ji-list lat) lat))
        (finitely-presented (sd-meet-fp-lat-p lat))
        (t (error "There is only code for testing sd-meet of ~
                  finite and finitely presented lattices."))))

(defun sd-join-p (&optional (lat *lat*))
  (case (lattice-type lat)
        ;;(congruence (sd-join-lat-p (lattice-mi-list lat) lat))
        (finite-lattice (sd-join-fin-lat-p (lattice-mi-list lat) lat))
        (finitely-presented (sd-join-fp-lat-p lat))
        (t (error "There is only code for testing sd-join of ~
                  finite and finitely presented lattices."))))

;;; This test if each join irreducible is join prime.
;;; Changed for our new representation of congruence lattices of lattices.
;;; Doesn't really depend on the exact rep.

(defun distributive-lat-p (jis lat &aux (jis2 jis))
  (let* ((lss (lattice-lssql lat))
	 (join (lattice-join lat)) )
    (loop
     (if (null jis2) (return t))
     (if (funcall lss
          (car jis2)
          (funcall join (set-difference jis (filter jis (car jis2) lat))))
         (return (values nil (car jis2))) )
     (pop jis2) ) ))

(defun distributive-fin-lat-p (jis lat &aux (jis2 jis))
  (loop
   (if (null jis2) (return t))
   (if (lssql
        (car jis2)
        (join (set-difference jis (filter jis (car jis2) lat)) lat) lat)
       (return (values nil (car jis2))) )
   (pop jis2) ) )

(defun sd-meet-fin-lat-p (jis lat &aux (jis2 jis))
  (loop
   (if (null jis2) (return t))
   (let* ((w (pop jis2)) 
	  (w* (join (remove w
			    (ideal jis w lat) :test #'equal) lat))
	  (kw (remove-if
	       #'(lambda (x) (lssql w (join (list w* x) lat) lat)) jis)))
	 (if (lssql w (join kw lat) lat)
	     (return (values nil w))))))

;;; This is based on Theorem 3 of my paper "Finitely presented lattices: 
;;; continuity and semidistributivity."

(defun sd-meet-fp-lat-p (lat)
  (let* ((p-meet (cons (lattice-one lat)
		       (close-under (generators lat) 
				    #'(lambda (&rest args) 
			             (funcall (lattice-meet lat) args)))))
	 (p-meet-lat (make-finite-lattice-from-grtrql
		      p-meet (lattice-grtrql lat))))
	(pprint p-meet-lat)
	(sd-meet-fin-lat-p (lattice-ji-list p-meet-lat) p-meet-lat)))

(defun sd-join-fp-lat-p (lat)
  (let* ((p-join (cons (lattice-zero lat)
		       (close-under (generators lat) 
				    #'(lambda (&rest args) 
			             (funcall (lattice-join lat) args)))))
	 (p-join-lat (make-finite-lattice-from-grtrql
		      p-join (lattice-grtrql lat))))
	(sd-join-fin-lat-p (lattice-mi-list p-join-lat) p-join-lat)))

(defun sd-join-fin-lat-p (mis lat &aux (mis2 mis))
  (loop
   (if (null mis2) (return t))
   (let* ((w (pop mis2)) 
	  (w* (meet (remove w
			    (filter mis w lat) :test #'equal) lat))
	  (kw (remove-if
	       #'(lambda (x) (grtrql w (meet (list w* x) lat) lat)) mis)))
	 (if (grtrql w (meet kw lat) lat)
	     (return (values nil w))))))

(defun ji-p (elt &optional (lat *lat*))
  (funcall (lattice-ji-p lat) elt) )

(defun ji (elt &optional (lat *lat*))
  (funcall (lattice-ji-p lat) elt) )

(defun mi-p (elt &optional (lat *lat*))
  (funcall (lattice-mi-p lat) elt) )

(defun mi (elt &optional (lat *lat*))
  (funcall (lattice-mi-p lat) elt) )

(defun cji-p (elt &optional (lat *lat*))
  (funcall (lattice-cji-p lat) elt) )


;(defmacro ji-p (elt &optional (lat '*lat*))
;  `(funcall (lattice-ji-p ,lat) ,elt) )
;
;(defmacro ji (elt &optional (lat '*lat*))
;  `(funcall (lattice-ji-p ,lat) ,elt) )
;
;(defmacro mi-p (elt &optional (lat '*lat*))
;  `(funcall (lattice-mi-p ,lat) ,elt) )
;
;(defmacro mi (elt &optional (lat '*lat*))
;  `(funcall (lattice-mi-p ,lat) ,elt) )
;
;(defmacro cji-p (elt &optional (lat '*lat*))
;  `(funcall (lattice-cji-p ,lat) ,elt) )

; I'm making this a function so it can be use in remove-if-not

(defun cji (elt &optional (lat *lat*))
  (funcall (lattice-cji-p lat) elt) )

;(defmacro cji (elt &optional (lat '*lat*))
;  `(funcall (lattice-cji-p ,lat) ,elt) )

(defun cmi-p (elt &optional (lat *lat*))
  (funcall (lattice-cmi-p lat) elt) )

(defun cmi (elt &optional (lat *lat*))
  (funcall (lattice-cmi-p lat) elt) )

;(defmacro cmi (elt &optional (lat '*lat*))
;  `(funcall (lattice-cmi-p ,lat) ,elt) )

(defun free-lattice-p (lat)
  (and (eql (lattice-type lat) 'finitely-presented)
        (every #'(lambda (x) (null (cdr (second x))))
               (lattice-filters lat))))

(defun free-over-poset-p (lat)
  (and (eql (lattice-type lat) 'finitely-presented)
        (null (lattice-join-table lat))
        (null (lattice-meet-table lat)) ))

;(defmacro free-lattice-p (lat)
;  `(and (eql (lattice-type ,lat) 'finitely-presented)
;        (every #'(lambda (x) (null (cdr (second x))))
;               (lattice-filters ,lat))))
;
;(defmacro free-over-poset-p (lat)
;  `(and (eql (lattice-type ,lat) 'finitely-presented)
;        (null (lattice-join-table ,lat))
;        (null (lattice-meet-table ,lat)) ))


(defun lower-atomic-p (w &optional (lat *lat*))
  (if (not (free-over-poset-p lat))
      (error "lower-atomic-p is only implemented for free lattices.~%~
        I couldn't find a nonexponential algorithm for fp lattices."))
  (cond
   ((ji-p w lat)
    (cji w lat))
   (t
    (every #'(lambda (x) (cji x lat)) (cdr w)))))

(defun upper-atomic-p (w &optional (lat *lat*))
  (if (not (free-over-poset-p lat))
      (error "upper-atomic-p is only implemented for free lattices.~%~
        I couldn't find a nonexponential algorithm for fp lattices."))
  (cond
   ((mi-p w lat)
    (cmi w lat))
   (t
    (every #'(lambda (x) (cmi x lat)) (cdr w)))))

(defun totally-atomic-p (w &optional (lat *lat*))
  (and (upper-atomic-p w lat) (lower-atomic-p w lat)))

;;; 6/22/93   added the test below to account for our congruence reps.

(defun semimodular-p (&optional (lat *lat*))
  (let* ((elems (univ lat))
	 (test (if (eq (lattice-type lat) 'congruence)
		   #'equal-congruence
		   #'equal)) )
        (loop
         (if (null elems) (return t))
         (let* ((covers (upper-covers (pop elems) lat))
                (covers2 covers))
               (loop
                (if (null covers) (return))
                (let* ((c (pop covers))
                       (c-covers (upper-covers c lat))
                       (covers3 covers2))
                      (loop
                       (if (null covers3) (return))
                       (if (eq c (car covers3))
                           nil
                           (if (not (member
                                     (join (list c (car covers3)) lat)
                                     c-covers
                                     :test test))
                               (return-from semimodular-p nil)))
                       (pop covers3) )))))))

(defun dual-semimodular-p (&optional (lat *lat*))
  (let* ((elems (univ lat))
	 (test (if (eq (lattice-type lat) 'congruence)
		   #'equal-congruence
		   #'equal)) )
        (loop
         (if (null elems) (return t))
         (let* ((covers (lower-covers (pop elems) lat))
                (covers2 covers))
               (loop
                (if (null covers) (return))
                (let* ((c (pop covers))
                       (c-covers (lower-covers c lat))
                       (covers3 covers2))
                      (loop
                       (if (null covers3) (return))
                       (if (eq c (car covers3))
                           nil
                           (if (not (member
                                     (meet (list c (car covers3)) lat)
                                     c-covers
                                     :test test))
                               (return-from dual-semimodular-p nil)))
                       (pop covers3) )))))))

(defun comparable-p (a b &optional (lat *lat*))
  (or (lssql a b lat) (lssql b a lat)))

(defun incomparable-p (a b &optional (lat *lat*))
  (not (or (lssql a b lat) (lssql b a lat))))



;(defmacro comparable-p (a b &optional (lat '*lat*))
;  `(or (lssql ,a ,b ,lat) (lssql ,b ,a ,lat)))

;(defmacro incomparable-p (a b &optional (lat '*lat*))
;  `(not (or (lssql ,a ,b ,lat) (lssql ,b ,a ,lat))))

(defun join-irredundant-p (set &optional (lat *lat*))
  (let* (
	 (join-front (lattice-zero lat)) )
	(loop
	 (if (null set) (return t))
	 (if (lssql (car set) (join (cons join-front (cdr set)) lat) lat)
	     (return 
	      (values nil 
	       (format nil "~s lies below the join of the others" (car set)))))
	 (setf join-front (join (list (pop set) join-front) lat)) )))


(defun meet-irredundant-p (set &optional (lat *lat*))
  (let* (
	 (meet-front (lattice-one lat)) )
	(loop
	 (if (null set) (return t))
	 (if (grtrql (car set) (meet (cons meet-front (cdr set)) lat) lat)
	     (return 
	      (values nil 
	       (format nil "~s lies above the meet of the others" (car set)))))
	 (setf meet-front (meet (list (pop set) meet-front) lat)) )))


;;; utility functions.

(defun pp-list (list)
  (dolist (x list nil)
          (pprint x)))
