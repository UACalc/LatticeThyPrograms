;;; freelat.lisp 1/26/91
;;;
;;; general stuff for free lattices
;;;
;;; 3/31/93 added lw to return the lattice L(w).

;;;(in-package :user)

(defun lw (w &optional (lat *lat*))
  (let* ((jw (j w lat))
	 (elems (cons (lattice-zero lat) (closure jw #'s)))) 
	(make-finite-lattice-from-grtrql elems (lattice-grtrql lat))))

(defun dag (w &optional (lat *lat*))
  (join (remove w (ideal (j w lat) w lat) :test #'equal) lat))

(defun k (w &optional (lat *lat*))
  (let* ((wdag (dag w lat))
	 (jw (j w lat))
	 (lst (set-difference jw (filter jw w lat) :test #'equal)))
	(remove-if #'(lambda (x) (lssql w (join (list wdag x) lat) lat))
		   lst)))

(defun cji-p-freelat (w &optional (lat *lat*))
  (if (ji-p w lat)
      (let* 
       ((j0 (remove w (j w lat))) 
	(j0-too j0) )
       (loop
	(if (null j0-too)
	    (return)
	    (if (not (cji-p-freelat (pop j0-too) lat))
		(return-from cji-p-freelat nil))))
       (let*  
	((dag (funcall (lattice-join lat) (ideal j0 w lat))) 
	 (k (remove-if #'(lambda (x) (lssql w (join (list x dag) lat) lat) ) 
		       (remove-if #'(lambda (y) (lssql w y lat)) j0) )) )
	(not (lssql w (join k lat) lat))))))

(defun cmi-p-freelat (w &optional (lat *lat*))
  (if (mi-p w lat)
      (let* 
       ((j0-dual (remove w (j-dual w lat))) 
	(j0-too j0-dual) )
       (loop
	(if (null j0-too)
	    (return)
	    (if (not (cmi-p-freelat (pop j0-too) lat))
		(return-from cmi-p-freelat nil))))
       (let*  
	((dag-dual (funcall (lattice-meet lat) (filter j0-dual w lat))) 
	 (k-dual (remove-if 
		  #'(lambda (x) (grtrql w (meet (list x dag-dual) lat) lat) ) 
		  (remove-if #'(lambda (y) (grtrql w y lat)) j0-dual) )) )
	(not (grtrql w (meet k-dual lat) lat))))))

(defun rank (w &optional (lat *lat*))
  (cond
   ((generator-p w lat) 1)
   ((or (eq (car w) 's) (eq (car w) 'p))
    (1+ (apply #'+ (mapcar #'(lambda (x) (rank x lat)) (cdr w)))))
   (t (error "w = ~s is not a proper word" w))))

(defun lower-median (&optional (lat *lat*) (gens (generators lat))) 
  (join (mapcar #'(lambda (x) (meet (remove x gens :test #'equal) lat))
	        gens) 
	lat))


(defun upper-median (&optional (lat *lat*) (gens (generators lat))) 
  (meet (mapcar #'(lambda (x) (join (remove x gens :test #'equal) lat)) 
		gens) 
	lat))

(defun kap (w &optional (lat *lat*))
  (if (generator-p w lat) 
      (values (car (kappa w lat)) (car (kappa-dual w lat)))
      (case (car w)
	    (p (car (kappa w lat)))
	    (s (car (kappa-dual w lat)))
	    (t (error "kap: w = ~s is not a lattice word" w)))))

(defun kap-dual (w &optional (lat *lat*))
  (car (kappa-dual w lat)))



; mu-sigma.lisp      10/29/90
; a quick implementation of mu and sigma from Freese Nation 1985

(defun mu (w var &optional (lat *lat*))
  (if (generator-p w lat) 
      (meet (list w var) lat) 
      (case (car w)
	    (p (meet (mapcar #'(lambda (x) (mu x var lat)) (cdr w)) lat))
	    (s (join (mapcar #'(lambda (x) (mu x var lat)) (cdr w)) lat)))))

(defun sigma (w var &optional (lat *lat*))
  (if (generator-p w lat) 
      (join (list w var) lat) 
      (case (car w)
	    (p (meet (mapcar #'(lambda (x) (sigma x var lat)) (cdr w)) lat))
	    (s (join (mapcar #'(lambda (x) (sigma x var lat)) (cdr w)) lat)))))


;;; dual.lisp 11/5/90
;;; a quick fix only good for free lattices.

(defun dual (w)
  (cond
   ((atom w) w)
   ((eql (car w) 's) (cons 'p (mapcar #'dual (cdr w))))
   ((eql (car w) 'p) (cons 's (mapcar #'dual (cdr w)))) ) )


; gentotatomic.lisp 11/90
;
; totally atomic elements
;
; This gives only one from each symmetry class. It depends on
; the current lattice being a free lattice with vars a subset 
; of its generators.

(defun gen-tot-atomic (vars)
  (gta (list (car vars)) (cdr vars)) )

(defun gta (list unused)
  (cond 
   ((null unused) list)
   (t
    (gta (nconc
	  (mapcar #'(lambda (x) (mu x (car unused))) list)
	  (mapcar #'(lambda (x) (sigma x (car unused))) list))
	 (cdr unused) ) ) ) )


;;; slip.lisp	12/09/93

;;; slim will give a cji slim element; see the monograph.
;;; lst should be a list of generators.

(defun slim (var lst &optional (lat *lat*) )
  (cond
   ((null lst) var)
   ((null (cdr lst)) (meet (list var (car lst)) lat))
   (t (meet (list var (join (list (car lst) 
			     (slim var (cdr lst) lat)) lat)) lat))))

;;; file semising.lisp 11/27/90

;(defun ss-p-old (w)
;  (cond
;   ((generator-p w) nil)
;   ((eql (car w) 's)  
;    (let* ((kd (car (kappa-dual w))))
;	  (pop w)
;	  (if (null kd) (return-from ss-p nil))
;	  (loop
;           (if (null w) (return nil))
;	   ;(format t "w is now ~S" w)
;           (if (lssql (car w) kd) (return (values t (car w))) )
;	   (pop w) )))
;   (t (let* ((k (car (kappa w))))
;	    (pop w)
;	    (if (null k) (return-from ss-p nil))
;	    (loop
;             (if (null w) (return nil))
;	     ;(format t "w is now ~S" w)
;             (if (lssql k (car w)) (return (values t (car w))))
;	     (pop w)) )) ) )

(defun ss-p (w &optional (lat *lat*))
  (cond
   ((generator-p w lat) nil)
   ((eql (car w) 'p)
    (if (cji-p w) 
	(filter (cdr w) (car (kappa w lat)) lat) ) )
   ((eql (car w) 's)
    (if (cmi-p w) 
	(ideal (cdr w) (car (kappa-dual w lat)) lat) ))))


; The next gives the middle element of a 3 elt interval as in the first
; example of section 10 of FreeseNation.
; The resulting elements are all semisingular (\kap(w) \leq w_i for some i).
; For half of them, their kappa is also semisingular. 
; make-ex constructs these.
; Here  q  is a totally atomic element.

(defun int3-1 (q)
  (if (and (eql (car q) 's) (> (length q) 2))
      (p q (apply #'s  (car (kappa-dual q)) (cddr q)))))

(defun int3-1-dual (q)
  (if (and (eql (car q) 'p) (> (length q) 2))
      (s q (apply #'p  (car (kappa q)) (cddr q)))))

; the second kind (see page 40)

(defvar dbar)
(defvar cbar)
(defvar bbar)

(let ((*lat* (make-free-lattice '(a b c d))))
     (setq dbar (p 'a 'b 'c))
     (setq cbar (p 'a 'b))
     (setq bbar (p 'a 'c)) )

; this is not the w_n in FN but the final answer which was denoted
; by plain  w. The  p_n is here  u_n and  x y z t   are changed to
; a b c d.
; These are semisingular in FL(4).

(defun w (n)
  (p 'd (s dbar (u n))))


(defun b (n)
  (cond 
   ((= n 0) 'b)
   (t (p 'b (s (c (- n 1)) cbar)))))

(defun c (n)
  (cond 
   ((= n 0) 'c)
   (t (p 'c (s (b (- n 1)) bbar)))))

(defun u (n)
  (mu (p (s (b n) bbar) (s (c n) cbar)) 'd))

;;;;;;;;;;;;;;;;;;

(defun bar (w)
  (cond
   ((generator-p w) nil)
   ((ji-p w) (lssql (p (second w) (car (kappa-dual (second w)))) w))
   ((mi-p w) (grtrql (s (second w) (car (kappa (second w)))) w)) ))


(defun foo (w p)
  (cond
   ((generator-p w) nil)
   ((ji-p w) nil)
   ((mi-p w) 
    (let ((kap (car (kappa-dual w))))
         (and
          (lssql (p w kap) p) 
          (lssql p w) ) ) ) ) ) 

(defun foo-list (w tas)
  (let* ((kap (car (kappa-dual w))) (kap* (p kap w)) (ans nil))
	(loop
	 (if (null tas) (return ans))
	 (if (and (lssql kap* (car tas)) (lssql (car tas) w))
	     (push (car tas) ans))
	 (pop tas))))

;;; (load "~ralph/Lisp/lat/Structlat/FreeLattices/close-filter")

(defun make-ex (q)
  (if (and (eql (car q) 'p) (> (length q) 2))
      (car (kappa-dual (s q (apply #'p  (car (kappa q)) (cddr q)))))))
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test a conjecture  ;;;;;;;;;;;;;;;
;;;
;;; Is  w_\dagger = \J {w_ij :  w_ij <= w}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-conj (w &optional (lat *lat*))
  (cond
   ((generator-p w) t)
   ((ji-p w)
    (let* ((kk nil)
	   (lst (cdr w)))
	  (loop
	   (if (null lst) (return))
	   (unless (generator-p (car lst))
		   (let ((lst2 (cdar lst))) 
			(loop
			 (if (null lst2) (return))
			 (if (lssql (car lst2) w lat)
			     (pushnew (car lst2) kk :test #'equal))
			 (pop lst2) ) ) )
	   (pop lst) )
	  (lssql (dag w lat) (cons 's kk) lat)))
   (t (error "w must be a meet"))))





