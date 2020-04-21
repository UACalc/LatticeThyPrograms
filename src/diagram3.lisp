;;; diagram3.lisp  5/12/91

;; This gives each element 3-dim coords. The third is determined by
;; my rank function. The other are determined by a dynamic system
;; of attractions to comparable elements and replusions from incomparable
;; elements.

;;  **************  Notes  ********************
;;
;;  The hexagon example shows that iterate1 can give an oscilation
;;  even is the *attraction and *replusiont-factor*'s are small.
;;  Also idivsion by 0 is possible.
;;  It appears that it might be better to fix one and use the new
;;  value when fixing the others. Right now are based only on the old
;;  coords.

;;;(in-package :user)

;;; delete this after it is debugged
 
(defmacro lhd (lat) `(lattice-hasse-diagram ,lat))


;;; *************** Attractions and Replusions ****************

;;; First the attraction and replusions stuff since it has some macros
;;; we need.

;; attrepl.lisp 4/91

;; Attraction is just proportional to the distance. It is a vector
;; from pt1 to pt2. It will be used with the first two coordinates
;; but this definition works fine for more coords.
;; For replusion the size is inversely proportional to the distance.

;; This version has the attraction being the x-y
;; projection of pt2 - pt1. Replusion is negative this vector divided
;; by the square of the distance between pt1 and pt2 (including the
;; third coord).
;; Both of these modify their third argument.

(defvar *attraction-factor* 0.1)

(defmacro attraction (pt1 pt2 current attraction-factor)
  `(let* (
          (p1 ,pt1)                      ;; prevent double evaluation
          (p2 ,pt2)                      ;; prevent double evaluation
	  (att ,attraction-factor)
          (dx (- (the float (first p2)) (the float (first p1))))
          (dy (- (the float (second p2)) (the float (second p1)))) )
	 (declare (float att dx dy))
         (setf
           (first ,current) (+ (the float (first ,current)) (* att dx))
           (second ,current) (+ (the float (second ,current)) (* att dy)))
         ,current))

;; This version of replusion is likely to give a divide by zero
;; error. Although it probably wouldn't occur if the data
;; had a slight perturbation. We could just put a check in.

(defvar *replusion-factor* 0.8)

;; An inverse squared version. 

(defmacro replusion (pt1 pt2 current replusion-factor)
  `(let* (
          (p1 ,pt1)                      ;; prevent double evaluation
          (p2 ,pt2)                      ;; prevent double evaluation
	  (rep ,replusion-factor)
          (dx (- (the float (first p1)) (the float (first p2))))
          (dy (- (the float (second p1)) (the float (second p2))))
	  (dz (- (the fixnum (third p1)) (the fixnum (third p2))) )
          (inv-d-cubed 
	   (if (and (< -0.2 dx 0.2) (< -0.2 dy 0.2) (= dz 0))
	       37.0 
	       (/ 1 (+ (expt (abs dx) 3) (expt (abs dy) 3)
			  ;;  we may want to scale this third part 
                          (expt (abs dz) 3))))))
	 (declare (float rep dx dy inv-d-cubed)
		  (fixnum dz))
         (setf
           (first ,current) (+ (the float (first ,current)) 
			       (* rep inv-d-cubed dx))
           (second ,current) (+ (the float (second ,current)) 
				(* rep inv-d-cubed dy)))
         ,current))


;;;; **************  Diagrams and Pictures   ******************

;;; 
;;;  Diagrams only have slots for things that don't change on 
;;;  drawings in different windows. 

(defclass diagram ()
 ((elems 	:accessor diagram-elems 	:initarg :elems)
  ;;(labels 	:accessor diagram-labels 	:initarg :labels)
  (upper-covers :accessor diagram-upper-covers 	:initarg :upper-covers)
  (comparables  :accessor diagram-comparables 	:initarg :comparables)
  (incomparables  :accessor diagram-incomparables :initarg :incomparables)
  (ranks 	:accessor diagram-ranks 	:initarg :ranks)
  (3d-coords 	:accessor diagram-3d-coords 	:initarg :coords)
  (generation 	:accessor diagram-generation 	:initform 0)
  ;;(rotation 	:accessor diagram-rotation 	:initform 0.0)
  ;;(tilt 	:accessor diagram-tilt 		:initform 0.0 
  ;;		  :initarg :tilt)
  ;;(2d-coords 	:accessor diagram-2d-coords)
  (lattice 	:accessor diagram-lattice 	:initarg :lattice)
  ))

;;; I am eliminating lin-ext since all lattice have their elements
;;; sort this way. 

(defun make-diagram (&optional (lat *lat*) (elems (univ lat) supplied))
  (let* ((d (make-instance 'diagram :elems elems :lattice lat)))
        (unless supplied (setf (lattice-hasse-diagram lat) d))
	d))

;;; Should check the delta after the 20 iterations. If it is too big
;;; we should print a warning and run another 20.

;(defvar *no-iterations* 50)
(defvar *no-iterations* 40)

(defmethod initialize-instance :after ((d diagram) &rest initargs) 
  (declare (ignore initargs))
  (let* ((elems (diagram-elems d))
	 (lat   (diagram-lattice d))) 
    (multiple-value-bind (co in uc) (setup-comparables elems lat) 
      (setf (diagram-ranks d)        (normalized-ranks lat elems)
	    (diagram-3d-coords d)    (setup-coords (diagram-ranks d))
	    (diagram-upper-covers d) uc
	    (diagram-comparables d)  co
	    (diagram-incomparables d)  in)
      (iterate (+ *no-iterations* (length elems)) d 
	       (* 0.5 *attraction-factor*)   
	       (* 3.0 *replusion-factor*))
      (iterate (+ *no-iterations* (length elems)) d 
	       (* 3.0 *attraction-factor*)   
	       (* 0.5 *replusion-factor*))
      (iterate (+ (* 4 *no-iterations*) (length elems)) d))))

;;; picture is a 2d projection of a diagram. This is what is actually
;;; displayed (with the function display) so it has slots for
;;; the process and the stream. Tilt is not use now. It is probably 
;;; a bad idea.

(defclass picture ()
 (
  (rotation 	:accessor picture-rotation 	:initform 0.0)
  (tilt 	:accessor picture-tilt 		:initform 0.0 
  		  :initarg :tilt)
  (2d-coords 	:accessor picture-2d-coords)
  (diagram	:accessor picture-diagram	:initarg :diagram)
  (mapped	:accessor mapped		:initform nil)
  (stream 	:accessor picture-stream	:initform nil)
  (process 	:accessor picture-process	:initform nil)
  ))

(defvar *all-pictures* nil)

(defun make-picture (diagram &optional angle tilt)
  (let* ((pic (make-instance 'picture :diagram diagram)))
	(if angle (setf (picture-rotation pic) angle))
	(if tilt  (setf (picture-tilt pic) tilt))
	(setf (picture-2d-coords pic) 
	      (proj-all-to-plane (diagram-3d-coords diagram)
				 (picture-rotation pic)
				 (picture-tilt pic)))
	;; (push pic *all-pictures*)
	pic))

;;; This uses vectors, but is only about 5% faster than the list version.
;;; But making the appropriate declarations here and in the attraction
;;; and replusion macros make it twice as fast.

(defmethod iterate (reps (d diagram) &optional (att *attraction-factor*) 
		                               (rep *replusion-factor*) )
  (let* ((coords (diagram-3d-coords d))
	 (n (length coords))
	 (oldcoords-vec (make-array n))
	 (newcoords-vec (coerce coords 'simple-vector))
	 comps
	 incomps
	 (fac (the float (sqrt n)))
	 (replusion (/ rep fac))
	 (att (/ att fac)))
   (declare (float fac att replusion)
	    (fixnum n))
   (incf (diagram-generation d) reps)
   (dotimes (i reps (setf (diagram-3d-coords d) (coerce newcoords-vec 'list)))
    (setf comps    (diagram-comparables d)
	  incomps  (diagram-incomparables d))
    (dotimes (j n) (setf (aref oldcoords-vec j) (aref newcoords-vec j)))
    (dotimes (j n)
      (let* ((pt (aref oldcoords-vec j))
             (comp (pop comps))
             (incomp (pop incomps))
             (force (list 0.0 0.0 0))
             (force
	      (dolist (k comp force)
               (attraction pt (aref oldcoords-vec k) force att)))
             (force
	      (dolist (k incomp force)
               (replusion pt (aref oldcoords-vec k) force replusion))) )
	    (setf (aref newcoords-vec j) (mapcar #'+ pt force)))))))
 
; ranks.lisp  6/21/87 modified for common lisp 7/17/90

; assumes that elem-lst  is a linear extension of the order.

;;; we need to add lat as a var to ranks

(defun normalized-ranks (&optional (lat *lat*) (elems (univ lat)))
  (let* ((depths (depths elems lat))) 
    (mapcar #'(lambda (x y) (+ (- x y) (first depths))) 
            (heights elems lat) depths ) ) )

(defun heights (elem-lst lat &aux
    heights ranked-lst)
  (setq heights (list 0) ranked-lst (list (pop elem-lst)))
  (loop
    (if (null elem-lst) (return heights))
    (rplacd (last heights) (list (height1 (car elem-lst)
                                           ranked-lst heights lat)))
    (rplacd (last ranked-lst) (list (pop elem-lst))) ) )

(defun height1 (elem elem-list heights lat &aux
    height)
  (setq height 0)
  (loop
    (if (null elem-list) (return height))
    (if (lssql (pop elem-list) elem lat)
      (setq height (max (+ 1 (pop heights)) height))
      (pop heights) ) ) )

; assumes that elem-lst is a linear extension.

(defun depths (elem-lst lat &aux
    depths ranked-lst)
  (setq
    elem-lst (reverse elem-lst)
    depths (list 0)
    ranked-lst (list (pop elem-lst)))
  (loop
    (if (null elem-lst) (return (nreverse depths)))
    (rplacd (last depths) (list (depth1 (car elem-lst)
                                           ranked-lst depths lat)))
    (rplacd (last ranked-lst) (list (pop elem-lst))) ) )


(defun depth1 (elem elem-list depths lat &aux
    depth)
  (setq depth 0)
  (loop
    (if (null elem-list) (return depth))
    (if (grtrql (pop elem-list) elem lat)
      (setq depth (max (+ 1 (pop depths)) depth))
      (pop depths) ) ) )

 
;; This returns 3 lists of lists. The ith of the first is a list
;; of positions of those elements comparable with (nth i elems),
;; (excluding the element itself).
;; The second is for the incomparables.
;; The third is the positions of the upper covers.

(defun setup-comparables (elems lat
        &aux (elems3 elems) comp incomp uc)
  (loop
   (if (null elems) (return (values (nreverse comp) 
				    (nreverse incomp)
				    (nreverse uc))))
   (let* ((elems2 elems3) (w (pop elems)) (k 0) (comp-w nil) (incomp-w nil)
	  (uc-w nil) (uc-w-elts (upper-covers w lat)))
         (loop
          (if (null elems2) (return))
          (cond
           ((equal w (car elems2)))
           ((comparable-p w (car elems2) lat) 
	    (push k comp-w)
	    (if (member (car elems2) uc-w-elts :test #'equal)
		(push k uc-w)) )
           ((incomparable-p w (car elems2) lat) (push k incomp-w)) )
          (pop elems2)
          (incf k))
         (push comp-w comp)
	 (push uc-w uc)
         (push incomp-w incomp)) ))

;; ranks is a nondecreasing list ranks.

(defmacro setup-radius (n)
  n )

(defvar *prime-list* )

(setq *prime-list* '(3 5 7 11 13 17 19 23 29 31))
;;; (setq *prime-list* '(3  7 13 19 29 ))
;;; (setq *prime-list* '(11 13 17 19 23 29 31))
(let* ((last (last *prime-list*)))
  (setf (cdr last) *prime-list*)
  nil)


(defun setup-coords (ranks &aux coords)
  (let ((primes *prime-list*))
  (loop
   (if (null ranks) (return (nreverse coords)))
   (let* ((rank (car ranks))
          (n (position rank ranks :test-not #'eql))
          (n (if (null n) (length ranks) n))
          (r (setup-radius n))
          (angle (/ (* 2 pi) n)) )
         (dotimes (i n)
           (push (list
                  (* r (cos (+ (* i angle) (/ pi (car primes)))))
                  (* r (sin (+ (* i angle) (/ pi (pop primes)))))
                  rank)
                 coords))
         (setq ranks (nthcdr n ranks))))))

(defun delta (coords1 coords2 &aux (ans 0))
  (loop
   (if (null coords1) (return ans))
   (incf ans (+
       (expt (- (first (first coords1)) (first (first coords2))) 2)
       (expt (- (second (first coords1)) (second (first coords2))) 2)))
   (pop coords1)
   (pop coords2) ))

;; Needed to changes the delta size (both *att..factor* and *rep ..factor*)
;; to avoid big oscillations. It is set according to the size.
;; prpbably should have a mul factor of 2 or 3.

(defun proj-to-plane (pt th alpha)
  (list (+ (* (first pt) (cos th)) (* (second pt) (sin th)) ) 
	(+ (* (first pt) (sin th) (sin alpha))
	   (* (second pt) (cos th) (sin alpha))
	   (* (third pt) (cos alpha)))))

(defun proj-all-to-plane (coords th &optional (alpha 0))
  (mapcar #'(lambda (x) (proj-to-plane x th alpha)) coords))

(defun di-make-uc (coords uc)
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (nth y coords)) x)) uc))

;;; This is taken from the file latwin-OWv3.lisp 

(defun format2  (stream &rest  args)
    (apply 'format stream args)
      (force-output stream) )

(defun ps-to-lisp (string)
  ;; declare string to be a simple string and the ans to be one.
  (read-from-string 
     (nsubstitute #\( #\[ (nsubstitute #\) #\] 
   (nsubstitute #\" #\( (nsubstitute #\" #\) string)))))) 

;;; need to worry about strings

(defun lisp-to-ps (obj &aux tmp)
  (setq obj (nsubstitute #\( #\{ (nsubstitute #\) #\}
	      (nsubstitute #\[ #\( (nsubstitute #\] #\)
                       (princ-to-string (lisp-to-ps-aux obj)))))))
  (loop
   (if (setq tmp (search "NIL" obj))
       (replace obj "[ ]" :start1 tmp)
       (return obj))))

(defun lisp-to-ps-aux (obj)
  (cond
   ((integerp obj)
    (concatenate 'string " " (princ-to-string obj) " ") )
   ((rationalp obj)
    (concatenate 'string (princ-to-string (numerator obj))
                         " "
                         (princ-to-string (denominator obj))
                         " div "))
   ((stringp obj)
    (concatenate 'string "{" obj "}") )
   ((atom obj) obj)
   (t (cons (lisp-to-ps-aux (car obj))
            (lisp-to-ps-aux (cdr obj))))))

;;; This is for drawing a finite subset of a lattice. Right now it 
;;; should be connected.

(defun draw-subset (elems &optional (lat *lat*))
  (draw-lattice (make-finite-lattice-from-grtrql elems (lattice-grtrql lat))))

;;; *********  The main function  ***************** ;;;

(defun draw-lattice (&optional (lat *lat*) (elems (univ lat) supplied-p))
  (when supplied-p
    (setf lat (make-finite-lattice-from-grtrql elems (lattice-grtrql lat))))
  (let* ((diagram (if (lattice-hasse-diagram lat) 
		      (lattice-hasse-diagram lat) 
		      (make-diagram lat))) 	; this also setf's
	 (pict (best-picture 			;   lattice-hasse-diagram
		  diagram
		  (incomparable-covers lat diagram)
		  (incomparables-same-rank diagram))) )
	(adjust-picture-ht-and-zero pict)
	(push pict *all-pictures*)
	(display pict)))

(defmethod display ((picture picture))
  (let* ((diagram (picture-diagram picture))
	 (2d-coords (picture-2d-coords picture)))
    (if (mapped picture) 
     (format2 (picture-stream picture) 
		 "~A /setdiagramlisp LatCan send /PaintAll Win send~%"
		 (lisp-to-ps 
		  (mapcar #'list 
	           2d-coords 
		   (di-make-uc 2d-coords (diagram-upper-covers diagram)))))
     (let* ((psh (run-program "psh" :input :stream :output :stream :wait nil))
      (proc (make-process 
		   :name "get lattice"
		   :stack-size 5000		; was 25000
		   :function #'adjust-picture 
		   :args (list picture) 
		   :wait-function #'listen 
		   :wait-args (list psh) ) ) )
      (setf (mapped picture) t
	    (picture-stream picture) psh 
	    (picture-process picture) proc )
      (format2 psh "/pwd (~a) def~%" (namestring (pwd)))
      (format2 psh "/userfilename () def /userlispfilename () def ~%")
      (format2 psh "/Lisp? true def ~%")
      (format2 psh "(/home/kahuna/ralph/NeWS/TNT/drawb.ps) run~%")
      (format2 psh "~A /setdiagramlisp LatCan send~%"
	       (lisp-to-ps 
	        (mapcar #'list 
		  2d-coords
		  (di-make-uc 2d-coords (diagram-upper-covers diagram)))))
      picture))))

;;; To debug a postscript error, change the "(setf command ...)" line
;;; to (setf command (read-line psh)) and add
;;; (format t "~a~%" command)

(defmethod adjust-picture ((pic picture))
  (let* ((psh (picture-stream pic))
	 (proc (picture-process pic))
	 command )
   (unwind-protect
    (loop
     (unless (listen psh)
         (process-wait "waiting for ps to repsond" #'listen psh) )
     (setf command (read-from-string (read-line psh)))
     (case command
	   (rotate (rotate pic)) 
	   (wider (wider pic)) 
	   (thinner (thinner pic)) 
	   (quit (kill-process proc) (return))
	   ))
    (setf (picture-process pic) nil
	  (mapped pic) nil
	  (picture-stream pic) nil) 
    (close psh))))

(defmethod rotate ((pic picture) &optional (angle (/ pi 19)))
  (with-accessors ((rotation  picture-rotation) 
		   (tilt      picture-tilt)
		   (2d-coords picture-2d-coords )
		   (diagram   picture-diagram)
		   (stream    picture-stream) ) 
		  pic
    (incf rotation angle) 
    (setf 2d-coords
	  (proj-all-to-plane (diagram-3d-coords diagram) rotation tilt))
    (adjust-picture-ht-and-zero pic)
    (when (mapped pic)
      (format2 
       stream "~A /setdiagramlisp LatCan send~%" 
       (lisp-to-ps
	(mapcar #'list 2d-coords 
		(di-make-uc 2d-coords (diagram-upper-covers diagram)))) )
      (format2 stream "/PaintAll Win send~%") )
    pic ) )

(defmethod rotate-1-rank (rank (d diagram))
  (let* ((coords (diagram-3d-coords d)))
	(loop
	 (if (null coords) (return d))
	 (let* ((triple (pop coords)))
	       (if (= rank (third triple))
		   (setf (first triple)  (- (first triple))
			 (second triple) (- (second triple))))))))

(defmethod wider ((pic picture) &optional (fac 1.1))
  (with-accessors ((2d-coords picture-2d-coords )
		   (diagram   picture-diagram)
		   (stream    picture-stream) ) 
		  pic
    (dolist (c 2d-coords)
      (setf (first c) (* fac (first c))))
    (when (mapped pic)
      (format2 
       stream "~A /setdiagramlisp LatCan send~%" 
       (lisp-to-ps
	(mapcar #'list 2d-coords 
		(di-make-uc 2d-coords (diagram-upper-covers diagram)))) )
      (format2 stream "/PaintAll Win send~%") )
    pic ) )

(defmethod thinner ((pic picture) &optional (fac 0.9090909090909091))
  (wider pic fac))

;;; ***************  Finding the best projection  ************** ;;;

(defun incomparable-covers (lat diagram)
  (let* ((ans nil))
	(dotimes (i (card lat))
		 (push (incomparable-covers-aux i lat diagram) ans))
	(nreverse ans)))

(defun incomparable-covers-aux (k lat diagram)
  (let* (
	 (ranks (diagram-ranks diagram))
	 (rank-elem (nth k ranks))
	 (lower-incomps nil)
	 (lower-incomps 
	  (dotimes (i (card lat) (nreverse lower-incomps))
		   (if (< (nth i ranks) rank-elem)
		       (push i lower-incomps))))
	 (upper-incomps nil)
	 (upper-incomps 
	  (dotimes (i (card lat) (nreverse upper-incomps))
		   (if (> (nth i ranks) rank-elem)
		       (push i upper-incomps))))
	 (upper-covers (diagram-upper-covers diagram))
	 (ans nil))
	(dolist (x lower-incomps) 
		(dolist (y upper-incomps)
			(if (member y (nth x upper-covers))
			    (push (list x y) ans))))
	ans ))

(defmacro incomparables-same-rank-aux (k diagram)
   `(remove-if-not #'(lambda (x) (= (nth x (diagram-ranks ,diagram))
				    (nth ,k (diagram-ranks ,diagram)) ))
		   (nth ,k (diagram-incomparables ,diagram))))

(defun incomparables-same-rank (diagram)
  (let* ((ans nil))
	(dotimes (i (length (diagram-elems diagram)))
		 (push (incomparables-same-rank-aux i diagram) ans))
	(nreverse ans)))

(defvar *infinity* 10000.0)

(defmethod badness ((p picture) incomp-covs incomp-same-rank)
  (let* ((b 0.0)
	 delta
	 (coords (picture-2d-coords p))
	 (n (length coords)))
	(declare (float delta b *infinity*) (fixnum n))
	(dotimes (i n)
	 (dolist (k (nth i incomp-same-rank))
	  (if (= (setq delta (abs (- (the float (first (nth i coords)))
				     (the float (first (nth k coords)))))) 0.0)
	      (incf b *infinity*)
	      (incf b (/ 1.0 delta)))))
	(labels 
	 ((dist2line (x x1 x2 h h1 h2)
           (declare (float x x1 x2 h h1 h2))
           (let* ((d (- h2 h1))) 
		 (abs (- x (/ (* x1 (- h2 h)) d) (/ (* x2 (- h h1)) d)))))) 
	 (dotimes (i n)
          (let* ((x (the float (first (nth i coords))))
		 (h (the float (second (nth i coords)))))
		(dolist (cov (nth i incomp-covs)) 
	         (let* ((dist (dist2line 
			       x 
			       (the float (first (nth (first cov) coords)))
			       (the float (first (nth (second cov) coords)))
			       h
			       (the float (second (nth (first cov) coords)))
			       (the float (second (nth (second cov) coords))))))
		       (declare (float dist))
		       (if (= dist 0.0)
			   (incf b *infinity*)
			   (incf b (/ 1 dist))))))))
	b))

(defvar *tries-for-best-picture* 30)

(defun best-picture (diagram incomp-covs incomp-same-rank)
  (declare (fixnum *tries-for-best-picture*))
  (let* ((p (make-picture diagram))
	 (min (badness p incomp-covs incomp-same-rank))
	 (angle (/ (* 2 pi) *tries-for-best-picture*))
	 badness
	 p2)
	(declare (float min angle badness)) 
	(dotimes (i *tries-for-best-picture*)
         (setf badness (badness
		        (setf p2 (make-picture 
				  diagram (* (the fixnum (+ 1 i)) angle)))
			incomp-covs incomp-same-rank))
         (when (< badness min)
	       (setf min badness
		     p p2)))
	p))


(defun adjust-picture-ht-and-zero (pict)
  (let* ((coords (picture-2d-coords pict))
	 (ht (second (car (last coords))))
	 (x0 (first (first coords))) )
	(if (> ht 8.0)
	    (let* ((e (expt 2 (ceiling (- (log ht 2) 3)))))
		  (dolist (c coords)
			  (setf (second c) (/ (second c) e)))))
	(dolist (c coords coords)
		(setf (first c) (- (first c) x0)))))




;;; ********** old stuff  ****************
 
;;; see diagram or diagram2.lisp for adjust-ps-diagram and various 
;;; saving functions.

