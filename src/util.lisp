; util.lisp

;  ***************  Utility LISP functions  ***************

;;;(in-package "USER")

(DEFUN MAKE-INF-CYCLE (X
      &aux A)
  (RPLACD (SETQ A (CONS X nil)) A) )

;;; general lattice functions
;;; 8/90 I now have mxl return two values: the maximal elements and 
;;; the remaining elements. 

(defun mxl (lst less-equal-test)
  (nmxl (copy-list lst) less-equal-test) )

(defun mnl (lst grtr-equal-test)
  (nmnl (copy-list lst) grtr-equal-test) )

;;; This form is to be compatable with the old version of mnl.
;;; It would make more sense to have both use a less-equal test.

(defun nmnl (lst grtr-equal-test)
  (nmxl lst grtr-equal-test) )

(defun nmxl (lst less-equal-test &aux last tail nonmaxs)
  (cond
   ((null lst) lst)
   (t 
    (loop
     (if (member (car lst) (cdr lst) :test less-equal-test) 
	 (push (pop lst) nonmaxs)
	 (return) ) )
    (setq last lst tail (cdr lst))
    (rplacd last nil)
    (loop
     (if (null tail) (return (values lst nonmaxs)))
     (cond
      ((member (car tail) (cdr tail) :test less-equal-test) 
       (push (pop tail) nonmaxs))
      ((member (car tail) lst :test less-equal-test) 
       (push (pop tail) nonmaxs))
      (t 
       (rplacd last tail)  ; put the list back together
       (pop tail)
       (pop last)
       (rplacd last nil) ) ) ) ) ) )

;;;  Linear extensions. order is a less-equal function.
;;;  The gyrations with chaning to gr-equal can be eliminated
;;;  when we make mnl use lssql.
;;;  The file lin-ext.lisp has a much faster topoloical sort, but it 
;;;  applies when we know the upper covers, which we don't when a lst
;;;  other than (univ lat) is given.

(defun linear-extension (lst &optional (lat *lat*))
  (lin-ext1 lst (lattice-grtrql lat)))

(defun lin-ext1 (lst grtr-fn)
  (cond 
   ((null lst) nil)
   (t 
    (multiple-value-bind (mins rest) 
			 (mnl lst grtr-fn) 
			 (nconc mins (lin-ext1 rest grtr-fn)) ) ) ) )

;;; Short name version.

(defun lin-ext (lst &optional (lat *lat*))
  (linear-extension lst lat))

;;; These version differed from Lucid to Franz CL in that when 
;;; :from-end was T, one applied  test(x,y) and teh other test(y,x).

;;; (DEFUN NMXL (LST &optional (le #'lssql))       ;the destructive version
;;; (DELETE-DUPLICATES
;;; (DELETE-DUPLICATES LST :test le :from-end T) :test le) )

;;; (DEFUN MXL (LST &optional (le #'lssql))       ;the nondestructive version
;;; (remove-DUPLICATES
;;;    (remove-DUPLICATES LST :test le :from-end T) :test le) )

;;;(DEFUN NMNL (LST &optional (ge #'grtrql))       ;the destructive version
;;;(DELETE-DUPLICATES
;;;(DELETE-DUPLICATES LST :test ge :from-end T) :test ge) )

;;;(DEFUN MNL (LST &optional (ge #'grtrql))       ;the nondestructive version
;;;(remove-DUPLICATES
;;;(remove-DUPLICATES LST :test ge :from-end T) :test ge) )

(defun filter (L W &optional (lat *lat*))
  "L is either a list of elements or a lattice, 
in which case the whole universe is used for L and LAT is L."
  (if (lattice-p l) (filter (univ l) w l) 
      (if (lattice-p lat) 
	  (remove-if #'(lambda (X) 
			 (not (funcall (lattice-lssql lat) W X))) l) 
	  (error "No current lattice"))))

(defun ideal (L W &optional (lat *lat*))
  "L is either a list of elements or a lattice, 
in which case the whole universe is used for L and LAT is L."
  (if (lattice-p l) (ideal (univ l) w l) 
      (if (lattice-p lat) 
	  (remove-if #'(lambda (X) 
			 (not (funcall (lattice-lssql lat) x W))) l) 
	  (error "No current lattice"))))

(defun interval (L v u &optional (lat *lat*))
  "L is either a list of elements or a lattice, 
in which case the whole universe is used for L and LAT is L."
  (if (lattice-p l) (interval (univ l) v u l) 
      (if (lattice-p lat) 
	  (remove-if #'(lambda (y)
			 (not (funcall (lattice-lssql lat) v y)))
	             (remove-if #'(lambda (X) 
			 (not (funcall (lattice-lssql lat) x u))) l)) 
	  (error "No current lattice"))))



;;; General closing functions

;;; closure really does the  same as close-under but does not print
;;; stats as it goes. This version uses a hash table to check if
;;; a value has already been found. 
;;; 
;;; I added a :max-size keyword to limit the size if desired.

(defun closure (set operation 
        &key (verbose nil) (filter nil) (max-size nil) (test #'equal))
  "The closure of a set under an operation taking multiple args."
  (if (or max-size filter verbose)
      (return-from closure (closure-x set operation max-size 
			:verbose verbose :filter filter :test test)))
  (if (null set) nil
      (let* ((ans (copy-list set))
	     (ht (make-hash-table :test test))
	     (ptr (last ans)) )
	    (dolist (x set) (setf (gethash x ht) t))
	    (labels
	     ((close-aux (list)
			 (cond ((null list) ans)
			       (t
				(close1 (car list))
				(close-aux (cdr list) ))))
	      (close1 (elt)
		      (let* (elt2
			     (ans2 ans) )
			    (loop
			     (when (null ans2) (return)) 
			     (setf elt2 (funcall operation elt (pop ans2))) 
			     (unless (gethash elt2 ht)
				     (setf (cdr ptr) (list elt2))
				     (setf ptr (cdr ptr))
				     (setf (gethash elt2 ht) t)) ))))
	     (close-aux (cdr set))))))

(defun closure-x (set operation max-size 
		&key (test #'equal) (verbose nil) (filter nil))
  "The closure up to max-size of a set under an operation taking multiple args."
  (if (null set) nil
      (let* ((ans (copy-list set))
	     (boolean t)
             (n (length set))
	     (ht (make-hash-table :test test))
	     (ptr (last ans)) )
	    ;;(if filter (setf set (remove-if-not filter set)))
	    (dolist (x set) (setf (gethash x ht) t))
	    (labels
	     ((close-aux (list)
			 (cond ((or (null list) 
				(and max-size (>= n max-size))) ans)
			       (t
				(if verbose 
				  (format t "~s, ~s~%" 
					(length list) (length ans)))
				(close1 (car list))
				(close-aux (cdr list) ))))
	      (close1 (elt)
		      (let* (elt2
			     (ans2 ans) )
			    (loop
			     (when (or (null ans2) 
				   (and max-size (>= n max-size))) 
				(return)) 
			     (setf elt2 (funcall operation elt (pop ans2)))
			     (if filter (setf boolean (funcall filter elt2)))
			     (unless (or (not boolean) (gethash elt2 ht))
				     (incf n)
				     (setf (cdr ptr) (list elt2))
				     (setf ptr (cdr ptr))
				     (setf (gethash elt2 ht) t)) ))))
	     (close-aux (cdr set))))))


;;; closure2 is the same as closure except operation takes a list
;;; of arguments, like, eg., (lattice-join <lat>).

(defun closure2 (set operation &key (test #'equal))
  "The closure of a set under an operation taking a list of args."
  (if (null set) nil
      (let* ((ans (copy-list set))
	     (ht (make-hash-table :test test))
	     (ptr (last ans)) )
	    (dolist (x set) (setf (gethash x ht) t))
	    (labels
	     ((close-aux (list)
			 (cond ((null list) ans)
			       (t
				(close1 (car list))
				(close-aux (cdr list) ))))
	      (close1 (elt)
		      (let* (elt2
			     (ans2 ans) )
			    (loop
			     (when (null ans2) (return)) 
			     (setf elt2 (funcall operation 
						 (list elt (pop ans2))))
			     (unless (gethash elt2 ht)
				     (setf (cdr ptr) (list elt2))
				     (setf ptr (cdr ptr))
				     (setf (gethash elt2 ht) t)) ))))
	     (close-aux (cdr set))))))

;(defun closure (set operation &key (test #'equal))
;  (if (null set) nil
;      (let* ((ans (copy-list set)))
;	    (labels
;	     ((close-aux (list)
;			 (cond ((null list) ans)
;			       (t
;				(close1 (car list))
;				(close-aux (cdr list) ))))
;	      (close1 (elt)
;		      (let* ((tmp nil)
;			     elt2
;			     (ans2 ans) )
;			    (loop
;			     (when (null ans2) (return))
;			     (unless 
;			      (or
;			       (member 
;				(setf elt2 (funcall operation elt (pop ans2))) 
;				ans :test test)
;			       (member elt2 tmp :test test))
;			      (push elt2 tmp)))
;			    (setf ans (nconc ans (nreverse tmp))))) )
;	     (close-aux (cdr set))))))


(DEFUN close-under (L &optional (operation #'s) &key (test #'equal) &aux
      M N)
  (SETQ N (COPY-LIST L) M (CDR L))
  (LOOP
    (if (NULL M) (return N))
    (format t "length m: ~S, ~T length n: ~S~%" (length m) (length n))
    ;;(format t "N:~%~s~%" n)
    (SETQ N (NCONC N (close-aux (POP M) N operation test)))))

(DEFUN close-aux (W L operation test &aux LF A L1)
  (let ((*check-elems* nil))
       (SETQ L1 L lf (list nil))
       (LOOP
        (if (NULL L) (return (CDR LF)))
        (if
         (and
          (NOT (MEMBER (SETQ A (funcall operation (POP L) W)) 
		       (cdr LF) :test test))
          (NOT (MEMBER A L1 :test test))) 
         (rplacd (last lf) (cons a nil) ) ) ) ) )


;Special closing 

(DEFUN closes (L &optional (lat *lat*))
  (if (lattice-p lat)
      (close-under l #'s)
      (error "No current lattice")) )

(DEFUN closep (L &optional (lat *lat*))
  (if (lattice-p lat)
      (close-under l #'p)
      (error "No current lattice")) )


(defun pre-in-list (list)
  (mapc #'(lambda (x) (print (pre-in x))) list)
  nil)

(defvar *pre-in-meet-sign* '||)	; could use '* 

(defun pre-in (w &optional not-top-level)
  (cond
   ((atom w)
    (format nil "~S" w))
   ((eql (car w) 's)
    (let ((temp (apply #'format nil (pre-in-aux (- (length w) 1) '+) 
		       (mapcar #'(lambda (x) (pre-in x t)) (cdr w)))))
       (if not-top-level 
	   (format nil "(~A)" temp)
	   temp)))
   ((eql (car w) 'p)
    (apply #'format nil (pre-in-aux (- (length w) 1) *pre-in-meet-sign*) 
           (mapcar #'(lambda (x) (pre-in x t)) (cdr w))))))


(defun pre-in-aux (n op)
  (if (eql n 2) 
      (concatenate 'string "~A" (format nil "~A" op) "~A")
      (concatenate 'string "~A" (format nil "~A" op) (pre-in-aux (- n 1) op))))

;;; some new printing stuff:
; pi.lisp 11/15/90
;;; changed pi to p-i because allegro objected to defining pi.

(defun p-i (w &optional par)
  (cond 
   ((atom w) (string-downcase (string w)))
   ((generator-p w)
    (if (and (= (length w) 2) (atom (car w)) (numberp (second w)))
	(concatenate 'string 
		     (string-downcase (first w))
		     "_" (prin1-to-string (second w)))) )
   ;; add case of more complicated generators
   ((eql (car w) 's)
    (cond
     ((= (length w) 1) "0")
     ((= (length w) 2)
      (p-i (second w)) )
     (t 
      (let ((string (concatenate 'string (p-i (second w) ) 
		 	                 " + " 
			                 (p-i (cons 's (cddr w))) ) ) ) 
           (if par (concatenate 'string "\(" string "\)") string) ) ) ) )
   ((eql (car w) 'p)
    (cond
     ((= (length w) 1) "1")
     ((= (length w) 2)
      (p-i (second w) t) )
     (t 
      (let ((string (concatenate 'string (p-i (second w) t) 
		 	                 ;; " \\meet " 
			                 (p-i (cons 'p (cddr w))) ) ) ) 
           (if par (concatenate 'string "\(" string "\)") string) ) ) ) )))

(defun pi-list (lst &optional (n 0))
  (cond
   ((null lst))
   (t (if (= n 0) (format t "~%"))
      (format t "~d~5t~a~%" n (p-i (car lst)))
      (pi-list (cdr lst) (1+ n))
      (values))))


(defmacro save-to-file (name outfile &optional comment)
  `(save-to-file-aux ',name ,name ,outfile ,comment) )

(defun save-to-file-aux (name obj outfile comment)
  (let ((*print-length* nil) (*print-level* nil) (*print-escape* t))
   (with-open-file (ofile outfile 
			 :direction :output 
			 :if-exists :append
			 :if-does-not-exist :create)
      (terpri ofile)
      (terpri ofile)
      (if comment (format ofile ";;; ~a~%" comment))
      (terpri ofile)
      (format ofile "(setq ~a (quote" name)
      (pprint obj ofile)
      (format ofile "~%))~%"))))
     
;;; 7/15/94 con failed for the one element lattice because it called
;;;         make-hash-table with size 0. The following hack fixes this.
;;;	    We should be careful not to use this for things that create 
;;;	    several hash tables like lssql.

(defun make-hash-table-safe (&rest keyword-pairs)
  (let* (
	 (keyword-pairs (copy-list keyword-pairs)) 
	 (tail (member :size keyword-pairs)))
	(if tail (setf (second tail) (max 1 (second tail))))
	(apply #'make-hash-table keyword-pairs) ))
