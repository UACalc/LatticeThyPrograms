;;; lssql.lisp 11/30/93

;;; 11/30/93 Using hash tables for both coordinates.
;;; Note: I also changed latcstr.lisp so that (lattice-hash-table lat)
;;; uses a #'eq table, since its entries are themselves hash tables.

;;; 5/9/92  I am clearing the hash table when lssql-fp is first called 
;;; if it is larger than 500. Note that in testing a huge word this is
;;; still ok since it will not clear the table in the middle.  
;;; Some tests showed that a large hash table slowed it down.

;;; 1/2 hash

;;; 11/21/90 I discovered the hash table was filling with
;;; equal elements. So I added :test #'equal to assoc and 
;;; puchnew when adding new elements.

;;;     We are replacing the test (atom v) with
;;; (funcall (lattice-generator-p *current-lattice*) v)
;;; it would be more efficient to get the list of gens at the
;;; beginning and just use a member test and declare it inline.
;;; This would be bad for free lattices. We could have a test
;;; for either atoms or member of the gen set. The best idea is
;;; problbly to check if (lattice-generators lat) returns nil
;;; and use (atom v) in this case; else test if v is a member of
;;; the generating set.
;;;     I have decided that this code will be for fp lattices and
;;; so will require (lattice-generators lat) be nonnil.
;;;     A good idea: make a lssql-hash-table part of a fp lattice.
;;;     This is now done; it was about 20% faster.

;;;(in-package :user)

(defvar *max-hash-size* 200)

(defun lssql-fp (v u &optional (lat *lat*))
  (let* ((*filters* (lattice-filters lat))
	 (*lookup-table* (lattice-hash-table lat))
         (*gens* (lattice-generators lat)) )
    (declare (special *filters* *lookup-table* *gens*))
    (if (> (hash-table-count *lookup-table*) *max-hash-size*) 
	(clrhash *lookup-table*))
    (lssql-lookup v u) ) )

(defun lssql-lookup (v u)
  (declare (special *lookup-table* *gens* *filters*)
	   ;(optimize (speed 3)) (inline lssql-gens)
           )
  (cond
    ((member v *gens* :test #'equal)
      (cond
        ((member u *gens* :test #'equal)
          (lssql-gens v u ))
        ((eq (car u) 'p)
           (lssql-v-all v (cdr u)) )
        (t (lssql-v-ex v (cdr u))) ) )
    ((eq (car v) 's)
      (lssql-all-u (cdr v) u) )
    ((member u *gens* :test #'equal)
      (lssql-ex-u (cdr v) u) )
    ((eq (car u) 'p)
      (lssql-v-all v (cdr u)) )
    (t
      (multiple-value-bind (ht-v found) (gethash v *lookup-table*)
        (if found
	    (multiple-value-bind (val found) (gethash u ht-v)
	      (if found (return-from lssql-lookup val)))))
      (lssql-meet-join v u) ) ) )

(defun lssql-record (v u)
  (declare (special *lookup-table* *gens* *filters*))
  (cond
    ((member v *gens* :test #'equal)
      (cond
        ((member u *gens* :test #'equal)
          (lssql-gens v u))
        ((eq (car u) 'p)
           (lssql-v-all v (cdr u)) )
        (t (lssql-v-ex v (cdr u))) ) )
    ((eq (car v) 's)
      (lssql-all-u (cdr v) u) )
    ((member u *gens* :test #'equal)
      (lssql-ex-u (cdr v) u) )
    ((eq (car u) 'p)
      (lssql-v-all v (cdr u)) )
    (t
      (let ((temp (lssql-meet-join v u)))
        (multiple-value-bind (ht-v y) (gethash v *lookup-table*)
          (if y
            (setf (gethash u ht-v) temp) 
	    (setf (gethash u (setf (gethash v *lookup-table*) 
				   (make-hash-table :test #'equal))) temp)))
        temp))))

(defun lssql-gens (v u)
  (declare (special *filters*))
  (or (equal v u) 
      (member u (cadr (assoc v *filters* :test #'equal)) :test #'equal)))


(defun lssql-v-all (v lst)
  (cond
    ((null lst) t)
    (t (and (lssql-record v (car lst))
            (lssql-v-all v (cdr lst)) ) ) ) )

(defun lssql-v-ex (v lst)
  (cond
    ((null lst) nil)
    (t (or (lssql-record v (car lst))
            (lssql-v-ex v (cdr lst)) ) ) ) )

(defun lssql-all-u (lst u)
  (cond
    ((null lst) t)
    (t (and (lssql-lookup (car lst) u)
            (lssql-all-u (cdr lst) u) ) ) ) )

(defun lssql-ex-u (lst u)
  (cond
    ((null lst) nil)
    (t (or (lssql-lookup (car lst) u)
            (lssql-ex-u (cdr lst) u) ) ) ) )

;;; this seems to be unnecessarily testing in the case of atoms
;(defun lssql-meet-join (v u)
;  (cond
;   ((intersect-p (cdr v) (cdr u)))
;   ((lssql-ex-u (cdr v) u))
;   ((lssql-v-ex v (cdr u))) ) )

(defun lssql-meet-join (v u)
  (declare (special *filters*) (special *gens*))
  (let* (
      (meetands (cdr v))
      (gen-joinands nil)
        ;; this might not be efficient
        ;; (remove-if-not #'(lambda (x) (member x *gens* :test #'equal)) (cdr u)))
      (gen-joinands2 (cdr u)) )
    (loop
      (if (null gen-joinands2) (return))
      (if (member (car gen-joinands2) *gens* :test #'equal)
        (push (pop gen-joinands2) gen-joinands)
        (pop gen-joinands2) ) )
    (loop
      (if (null meetands) (return))
      (cond
        ((member (car meetands) *gens* :test #'equal)
          (setq gen-joinands2 gen-joinands)
          (loop
            (if (null gen-joinands2) (return))
            (if (and (member (car gen-joinands2) *gens* :test #'equal)
		     (lssql-gens (car meetands) (car gen-joinands2)))
              (return-from lssql-meet-join t) )
            (pop gen-joinands2) ) ) )
      (pop meetands) ) )
  (let ((elts (cdr v)))
    (loop
      (if (null elts) (return))
      (cond
        ((member (car elts) *gens* :test #'equal)
          (pop elts))
        ((lssql-lookup (pop elts) u) (return-from lssql-meet-join t)) ) ))
  (let ((elts (cdr u)))
    (loop
      (if (null elts) (return))
      (cond
        ((member (car elts) *gens* :test #'equal)
          (pop elts))
        ((lssql-lookup v (pop elts)) (return-from lssql-meet-join t))))))


;(defun intersect-p (lst1 lst2)
;  (cond
;    ((null lst1) nil)
;    ((member (car lst1) lst2 ) t)
;    (t (intersect-p (cdr lst1) lst2 )) ) )
