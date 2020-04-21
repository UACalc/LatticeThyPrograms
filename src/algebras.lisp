; algebras.lisp   7/7/90
; revised 	12/27/94 to use a bit-array for storing "images".
; revised 	2/6/95   to use the new principal-congruence function

;;; This uses lisp structures for algebras.

;;;(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))


(defvar *alg*)    ; the default lattice

(defstruct (algebra 
	    (:constructor create-algebra)
	    (:print-function 
	     (lambda (lat stream depth)
		     (declare (ignore depth))
		     (if *print-pretty* 
			 (print-algebra lat stream)
			 (print-algebra-short lat stream)))))
  type          ; free, finitely presented, finite, congruence, etc
  lock          ;; for the new multitasking
  elem-list     ;; a list of elements sorted by general-lex-order
  elem-p        ;; recognizer fn
  elem-ht	;; a hash table returning the position of x in  elem-list
  elem-vector	;; a vector whose ith entry is the ith element of elem-list
  operations	;; a list of the operations as lisp functions
  arities	;; the corresponding arities
  ;var-lists	;; a list of the list of variables for the operations
  names		;; a list of names of the operations (optional)
  constants	;; a list of the constants (possibly delete this)
  generators    ;; list
  image-bit-array ;; a matrix of bits indexed by the two element subsets
		;; of the algebra (represented as a list: (x y) ). The
		;; (x y), (u v) entry is 1 iff there
		;; is a unary alg fn  f  with  {f(x), f(y)} = {u, v}
		;; (u v)  and  (x y)  must be ordered by genral-lex-order
  con		;; the congruence lattice
  principal-congruences ;; the principal congruences. Congruences represented
		   ;; as vectors.
  ji-congruences   ;; join irreducible congruences as vectors.
  (pprint-elem 'identity)
  )

;;; fancy lattice printing

(defun print-algebra-short (lat stream)
  (declare (ignore lat))
  (format stream "#<algebra>" ))

(defun print-algebra (lat &optional (stream t))
  (format stream "algebra:~%")
  (if (algebra-elem-list lat)
      (progn
       (format stream "~t elements:~15t ~s~%" (algebra-elem-list lat))
       (format stream "~t size:~15t ~d~%" (length (algebra-elem-list lat)))))
  (if (algebra-generators lat)
    (format stream "~t generators:~15t ~s~%" (algebra-generators lat)))
  (if (algebra-arities lat)
    (format stream "~t arities:~15t ~s~%" (algebra-arities lat)))
  (if (algebra-con lat)
      (progn
       (format stream "~t con:~%")
       (if (lattice-elem-list (con lat))
           (format stream "~5t size:~15t ~d~%" (card (con lat))) )
           (format stream "~5t no. ji members:~15t ~d~%"
            (length (lattice-ji-list (con lat)))) ) ) )

(defun set-elem-ht (elems alg)
  (let ((ht (make-hash-table :test #'equal))
	(k -1) )
       (dolist (x elems) (setf (gethash x ht) (incf k))) 
       (setf (algebra-elem-ht alg) ht) ))

(defun make-algebra (elems ops arities)
  (let* ((alg (create-algebra))
	 (k -1)
	 (elems (sort (copy-list elems) #'general-lex-order))
	 (vector (make-array (length elems) :initial-contents elems))
	 (ht (make-hash-table :test #'equal)) )
	(dolist (x elems) (setf (gethash x ht) (incf k)))
	(setf
	 (algebra-elem-list alg) elems
	 (algebra-elem-vector alg) vector
	 (algebra-arities alg) arities
	 (algebra-elem-ht alg) ht
	 (algebra-operations alg) ops )
	alg ))

(defun insert-nth (item list n)
  (declare (fixnum n) (list list))
  (cond
   ((and (integerp n) (>= n 0))
    (cond 
     ((zerop n)
      (cons item list)) 
     ((> n (the fixnum (length list))) list)
     (t 
      (let ((temp (nthcdr (the fixnum (- n 1)) list))) 
           (rplacd temp (cons item (cdr temp)))
	   list ) ) ) ) ) )

(defun make-images (alg)
  (let* ((firsts (algebra-elem-list alg))
         (seconds firsts)
	 (n (length firsts))
	 (m (/ (* n (- n 1)) 2))
	 (index -1)
	 (ht (algebra-elem-ht alg))
         (ops (algebra-operations alg))
         (arities (algebra-arities alg))
         ;; (var-lists (algebra-var-lists alg))
         (image-bit-array (make-array (list m m) 
			   :element-type 'bit :initial-element 0)) )
        (dolist (x firsts image-bit-array)
          (pop seconds)
          (dolist (y seconds)
            (make-images2 x y (incf index)
			  n ht ops arities firsts image-bit-array)))))


(defun make-images2 (x y index n ht ops arities elems image-bit-array)
  (declare (list elems arities ops))
  (let* (
	 (count 0)
	 (m (the fixnum (/ (the fixnum (* n (the fixnum (- n 1)))) 2))))
	(declare (fixnum n count)) 
	(loop
	 (if (null ops) (return image-bit-array))
	 (if (= count m) (return image-bit-array))
	 (setq count (make-images3 x y index n ht
				    (pop ops)
				    (pop arities)
				    elems image-bit-array m count)))))


(defun make-images3 (x y index n ht op arity elems image-bit-array m count
		       &aux (pos 0) u v)
  (declare (list elems) (fixnum arity pos))
  (loop
   (if (= pos arity) (return count))
   (let (
	 index2
	 (vector1 (make-list arity :initial-element elems))
	 (vector2 (make-list arity :initial-element (car elems)))
	 (vector3 (make-list arity :initial-element (car elems))) )
	(setf (nth pos vector2) x
	      (nth pos vector3) y)
	(loop
	 (unless (equal (setq u (apply op vector2))
	                 (setq v (apply op vector3)) )
             (setf u (gethash u ht) v (gethash v ht)) 
	     (if (> u v) (psetf u v v u))
;;(format t "u and v and n are ~s ~s ~s~%" u v n)
             (setf index2 (+ (* u n) (- (/ (* u (+ u 1)) 2)) v (- u) -1)) 
	     (if (= 0 (aref image-bit-array index index2)) (incf count))
	     (if (= count m) (return-from make-images3 count))
	     (setf (aref image-bit-array index index2) 1))
	 (if (not (incq-vector3-with-fixed vector1 vector2 vector3 elems pos))
	     (return t)) ) )
   (setq pos (the fixnum (+ 1 pos))) ) )

;;; This has the side effect of incrementing  vector2
;;; and  vector3 through all possible
;;; ways of of filling it with elements from  elems,  except that
;;; position  fixed-pos  stays fixed.
;;; In the same place  vector3  has another fixed element of the algebra.
;;; We could replace fixed-pos with a list of positions which are fixed.
;;; The function returns  t  or  nil  when it is done.

(defun incq-vector3-with-fixed (vector1 vector2 vector3 elems fixed-pos)
   (declare (list vector1 vector2 vector3 elems) (fixnum fixed-pos))
   (cond
      ((null vector1) nil)
      ((zerop fixed-pos)
        (incq-vector3-with-fixed (cdr vector1) (cdr vector2) (cdr vector3)
                               elems -1) )
      ((identity (cdar vector1))
        (rplaca vector1 (cdar vector1))
        (rplaca vector2 (caar vector1))
        (rplaca vector3 (caar vector1))
         't)
      (t
        (rplaca vector1 elems)
        (rplaca vector2 (car elems))
        (rplaca vector3 (car elems))
        (incq-vector3-with-fixed (cdr vector1) (cdr vector2) (cdr vector3)
                               elems (the fixnum (- fixed-pos 1))) ) ) )

;;; This is in congrlat.lisp

;;; (defun cg (a b alg-or-lat)
;;;   (if (algebra-p alg-or-lat) (cg-alg a b alg-or-lat)) )

(defun cg-alg (a b alg)
  (if (equal a b) 
      nil
      (let* ((n (card alg))
	     (i (gethash a (algebra-elem-ht alg)))
	     (j (gethash b (algebra-elem-ht alg))) )
	(if (> i j) (psetf i j j i))
	(if (not (algebra-principal-congruences alg))
	    (setf (algebra-principal-congruences alg)
		  (principal-congruences alg)))
        (part-vec-2-part-block 
         (aref (algebra-principal-congruences alg) 
	       (+ (* i n) (- (/ (* i (+ i 1)) 2)) j (- i) -1))))))

(defun congs (alg)
  "Gives a list of the pricipal congruences in block form."
  (if (not (algebra-principal-congruences alg))
      (setf (algebra-principal-congruences alg)
	    (principal-congruences alg)))
  (let* ((n (card alg))
	 (ans nil)
	 (ans2 nil)
	 (elem-vector (algebra-elem-vector alg))
	 (prins (algebra-principal-congruences alg)) ) 
	(dotimes (i (/ (* n (- n 1)) 2))
	   (pushnew (aref prins i) ans :test #'equalp))
;(format t "number of distinct prins is ~d~%" (length ans))
	(dolist (x ans)
	   (pushnew (part-vec-2-part-block x) ans2 :test #'equal))
	(labels ((foo (block)
		      (unless (null block)
			(setf (car block) (aref elem-vector (car block)))
			(foo (cdr block)))) )
		(dolist (x ans2 ans2)
		  (mapcar #'foo  x) ))	; assumes elements already sorted
	(values ans2 ans)))



;;; dirprod.lisp  7/26/90
;;; 
;;; A version with arbitrarily many factors

(defun make-direct-product (&rest lst)     ; a nonnull list of algebras
 (let* (
	(elems (sort (direct-prod-sets 
		      (mapcar #'(lambda (x) (algebra-elem-list x)) lst)) 
		     #'general-lex-order))
	(ops (direct-prod-ops 
	      (mapcar #'(lambda (x) (algebra-operations x)) lst)
	      elems) )
        (arities1 (algebra-arities (car lst))) )
       (if (not (check-arities arities1 (cdr lst)))
           (error "The algebras are not of the same type.") )
       (make-algebra elems ops arities1)))

(defun check-arities (arity lst)
  (every #'(lambda (x) (equal arity (algebra-arities x))) lst) )

(defun direct-prod-sets (sets)
  (cond
   ((null (cdr sets)) (mapcar #'list (car sets)))
   ((null sets) nil)
   (t
    (mapcan #'(lambda (x) (mapcar #'(lambda (y) (cons x y)) 
				  (direct-prod-sets (cdr sets)))) 
	    (car sets) )) ) )

;;; I added elems so that the operations would return shared elems,
;;; so make-images would use much less space.
;;; I made it optional for use with free algebra, but later decided
;;; not to use it.

(defun direct-prod-ops (ops &optional elems)  ; a list of lists of ops
  (dirprod-ops2 (transpose ops) elems) )

; (defun dirprod-ops2 (ops)
  ; (cond
   ; ((null ops) nil)
   ; (t
    ; (let ((op (car ops)))
       ; (cons #'(lambda (&rest args)
		; (mapcar #'eval (transpose (cons op args))) ) 
	     ; (dirprod-ops2 (cdr ops)) ) ) ) ) )

;(defun dirprod-ops2-old (ops)
  ;(cond
   ;((null ops) nil)
   ;(t
    ;(let ((op (car ops)))
       ;(cons #'(lambda (&rest args)
		;(let ((opps op) (tr-args (transpose args)) (ans nil))
		     ;(loop
		      ;(if (null opps) (return (nreverse ans)))
		      ;(push (apply (pop opps) (pop tr-args)) ans) ) ))
	     ;(dirprod-ops2 (cdr ops)) ) ) ) ) )


(defun dirprod-ops2 (ops &optional elems)
  (cond
   ((null ops) nil)
   (t
    (let ((op (car ops))) ; really op is a list of corresponding ops
	 (if elems
             (cons (compile nil `(lambda (&rest args)
		(let ((opps ',op) (tr-args (transpose args)) (ans nil))
		     (loop
		      ;; (if (null opps) (return (nreverse ans)))
		      (if (null opps) 
			  (return (find 
				   (nreverse ans) ',elems :test #'equal)))
		      (push (apply (pop opps) (pop tr-args)) ans) ) )))
		   (dirprod-ops2 (cdr ops) elems) )
             (cons (compile nil `(lambda (&rest args)
		(let ((opps ',op) (tr-args (transpose args)) (ans nil))
		     (loop
		      (if (null opps) (return (nreverse ans)))
		      (push (apply (pop opps) (pop tr-args)) ans) ) )))
	           (dirprod-ops2 (cdr ops)) ) ) ) ) ) )

(defun transpose (lst)
  (cond
   ((null lst) nil)
   ((null (car lst)) nil)
   (t (cons (mapcar #'car lst) (transpose (mapcar #'cdr lst))) ) ) )

;;; A test of lexical scoping.

;(defun foo (n)
  ;(let ((m 0) (ans nil))
       ;(loop
	;(if (>= m n) (return ans))
	;(let ((k m))
	     ;(push #'(lambda () k) ans)
	     ;(incf m) ) ) ) )

;;; The old version which I'll keep since the new version is hairy 
;;; and this might be more efficient for 2 algs.

(defun make-direct-product2 (alg1 alg2)
  (let ((temp (create-algebra))
        (arities1 (algebra-arities alg1))
	(k -1)
	(ht (make-hash-table :test #'equal)) )
    (if (not (equal arities1 (algebra-arities alg2)))
      (error "The two algebras are not of the same type.") )
    (setf
      (algebra-elem-list temp) (sort (direct-prod-sets2
                                       (algebra-elem-list alg1)
                                       (algebra-elem-list alg2))
                                 #'general-lex-order)
      (algebra-elem-vector temp) 
	(make-array (length (algebra-elem-list temp)) 
				  :initial-contents (algebra-elem-list temp))
      (algebra-arities temp) arities1
      (algebra-operations temp) (direct-prod-ops2
                                  (algebra-operations alg1)
                                  (algebra-operations alg2) ) )
    (dolist (x (algebra-elem-list temp)) (setf (gethash x ht) (incf k)))
    (setf (algebra-elem-ht temp) ht)
    temp ) )


(defun direct-prod-sets2 (set1 set2)
  (cond
    ((null set1) nil)
    (t (append (dir-prod-sets2-aux (car set1) set2)
               (direct-prod-sets2 (cdr set1) set2) )) ) )

(defun dir-prod-sets2-aux (elt set)
  (mapcar #'(lambda (y) (list elt y)) set) )

(defun direct-prod-ops2 (ops1 ops2)
  (mapcar #'(lambda (x y)
      #'(lambda (&rest args)
          (list
            (apply x (mapcar #'first args))
            (apply y (mapcar #'second args)) ) ) ) ops1 ops2) )

;;; Integers mod n

(defun make-int-mod-n (n)
  (declare (fixnum n))
  (let ((temp (create-algebra))
	(ht (make-hash-table :test #'equal))
	(elems (make-int-mod-aux n)) )
       (setf (algebra-operations temp) (list #'(lambda (x y) 
					(declare (fixnum x y))
					(the fixnum 
					 (mod (the fixnum (+ x y)) n))))
	     (algebra-elem-list temp) elems
	     (algebra-elem-vector temp)
		 (make-array (length elems) :initial-contents elems)
	     (algebra-arities temp) '(2) )
       (dolist (x elems) (setf (gethash x ht) x))
       (setf (algebra-elem-ht temp) ht)
       temp ) )

(defun make-int-mod-aux (n &aux ans)
  (declare (fixnum n))
  (loop
    (setq n (- n 1))
    (push n ans)
    (if (= n 0) (return ans)) ) )


;;;  * * * *   Subalgebras   * * * *

;;; Using the alvin notation,  sg(X)  for subalgebra generated by  X.

(defun sg-alg (gens alg)
  (sort (sg-aux gens (algebra-operations alg) (algebra-arities alg))
	#'general-lex-order))

(defun make-sg-alg (gens alg)
  (let ((temp (create-algebra)))
       (setf
	(algebra-generators temp) gens 
	(algebra-elem-list temp) (sg-alg gens alg)
	(algebra-elem-vector temp) 
	  (make-array (length (algebra-elem-list temp)) 
		      :initial-contents (algebra-elem-list temp))
	(algebra-arities temp) (algebra-arities alg)
	(algebra-operations temp) (algebra-operations alg) )
       (set-elem-ht (algebra-elem-list temp) temp)
       temp) )


(defun sg-aux (gens ops arities
    &aux ans)
  (setq gens (remove-duplicates gens :test #'equal))
  (loop
    (setq
      ans gens
      gens (close1-list gens ops arities ans))
    ;; (print 111111111111)
    ;; (print (length gens))
    (if
     (equal gens ans) (return gens) ) ) )

(defun close1-list (set ops arity-list &optional ans)  ;; ans is optional
  (loop
    (if (null ops) (return ans))
    (setq ans (close1 set (pop ops) (pop arity-list) ans)) ) )

;;; close1 [set f vars] returns the set of f(x1,...,xn) with xi in  set

(defun close1 (set op k &optional ans  ;; ans is optional
    &aux vector1 vector2)
  (setq
    vector1 (make-list k :initial-element set)
    vector2 (make-list k :initial-element (car set)) )
  (loop
;(format t "op is ~S~%" op)
;(format t "vector2 is ~S~%" vector2)
;(format t "result is ~S~%~%" (apply op vector2))
    (pushnew (apply op vector2) ans :test #'equal)
    ;; (print (length ans))
    (if (not (incq-vector2 vector1 vector2 set)) (return ans)) ) )

;;; gens-list is a list of all sets which generate the whole alg.

(defun k-gen-subalgebras (k alg &optional gens-list)
  (unless (and (integerp k) (<= 1 k (card alg)))
          (error "The first argument must be between 1 and ~D." (card alg)))
  (unless (algebra-p alg)
          (error "The second argument must be an algebra."))
  (let* ((set (universe alg))
         (size (card alg))
(n 0)
         (gens (copy-list (nthcdr (- size k) set)))
         (ans nil) )
        (loop
(print (incf n))
         (if (member gens gens-list
                          :test #'(lambda (x y) (subsetp y x)))
	     (pushnew set ans)
             (let ((subuniv (sg-alg gens alg)))
                  (if (= size (length subuniv))
		      (progn
                        (push (copy-list gens) gens-list)
	                (pushnew set ans))
		      (pushnew subuniv ans :test #'equal)) ) )
         (if (not (setq gens (next-subset-aux gens set)))
	     (return (values ans gens-list)) )) ) )


;;;    * * * *   Homomorphisms   * * * *

(defun make-homomorphic-image (alg congr)
  (let ((temp (create-algebra)))
       (setf
	(algebra-elem-list temp) 
	  (sort 
	   (append (mapcar #'list 
			   (set-difference (algebra-elem-list alg) 
					   (apply #'append congr) 
					   :test #'equal )) 
		   congr) #'general-lex-order)
	(algebra-elem-vector temp) 
	  (make-array (length (algebra-elem-list temp)) 
		      :initial-contents (algebra-elem-list temp))
	(algebra-arities temp) (algebra-arities alg)
	(algebra-operations temp) 
	  (mapcar #'(lambda (x) 
	             #'(lambda (&rest args)
			(find (apply x (mapcar #'first args))
			      (algebra-elem-list temp) 
			      :test #'(lambda (y z) (member
						    y z :test #'equal)))) )
		  (algebra-operations alg) ) )
       (set-elem-ht (algebra-elem-list temp) temp)
       temp) )




;;;    * * * *   Homomorphism problem   * * * *

;;; When do a map from a generating set of  A  into  B  extend to
;;; a homomorphism? We will usually be interested in the endomorphism
;;; problem, so the ops and arities of the two algebras will be the
;;; same.

;;; Below mapping is a list of elements of the form  (a . phi(a)),
;;; where phi is the (partial) proposed homo.

(defvar *history*)

(defun homo-p (mapping ops1 ops2 arities
    &aux mapping2)
  (if (algebra-p ops1) (setf ops1 (algebra-operations ops1)))
  (if (algebra-p ops2) (setf ops2 (algebra-operations ops2)))
  (setf *history* nil)
  (cond
   ((not (mapping-p mapping)) nil)
   (t (loop
    (setq
      mapping2 mapping
      mapping
        (close-homo-list (mapcar #'car mapping) mapping ops1 ops2 arities))
    (if (null mapping) (return nil))
    (if (equal mapping2 mapping) (return mapping)) ) ) ) )

(defun mapping-p (map)
  (loop
   (cond
    ((null map) (return t))
    ((member (pop map) map :test
        #'(lambda (x y) (and (equal (car x) (car y))
            (not (equal (cdr x) (cdr y))))))
      (return nil)) ) ) )

(defun close-homo-list (set mapping ops1 ops2 arities)
  (loop
    (if (null ops1) (return mapping))
    (if (not (setq mapping
          (close-homo set mapping (pop ops1) (pop ops2) (pop arities))))
      (return nil) ) ) )

(defvar *print-homo* t)

(defun close-homo (set mapping op1 op2 k
    &aux vector1 vector2 elem image )
  (setq
    vector1 (make-list k :initial-element set)
    vector2 (make-list k :initial-element (car set)) )
  (loop
    (setq
      elem (apply op1 vector2)
      image (apply op2
        (mapcar
          #'(lambda (x)
            (cdr (find x mapping :test #'(lambda (x y) (equal x (car y))))))
          vector2) ) )
;    (spaces 20) (print (length mapping))
    (when (not (member elem mapping :test #'(lambda (x y) (equal x (car y))))) 
	  (push (cons elem image) mapping)
	  (push (list '= (cons op1 (copy-tree vector2)) elem) *history*))
    (if (not (member (cons elem image) mapping :test #'equal)) 
        (progn
	  (if *print-homo* 
           (format t "screw-up: ~s~%args: ~s~%map: ~s~%" 
               (cons elem image) vector2 mapping))
	  (return nil) ))
    (if (not (incq-vector2 vector1 vector2 set)) (return mapping)) ) )


;;;   * * * *    Free algebras    * * * *

;;; 09/03/99 I am allowing alg to be a list of algebras. 
;;;	     This does not test much redundancy now.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; not complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defun diagonal-subdirect-product (lst)
;   (let ((temp (create-algebra))
; 	(n (length (algebra-generators (first lst))))
; 	(ops nil)
; 	(gens nil) )
;        (dotimes (i n (setf gens (nreverse gens)))
; 	(push (mapcar #'(lambda (x) (nth i (algebra-generators x))) lst) gens))
;        (setf
; 	(algebra-generators temp) gens
; 	(algebra-arities temp) (algebra-arities (first lst))

(defun make-free-algebra (n alg &optional elems-p)
  ;;here
  ;;;(if (not (algebra-p alg)) (make-free-algebra2 n 
  ;;; ))
  ;;here
  (if (or (not (integerp n)) (< n 1)) (error "n  must be at least 1."))
  (let ((temp (create-algebra))
	(gens (make-free-algebra-aux n alg)) )
       (setf
	(algebra-generators temp) gens
	(algebra-arities temp) (algebra-arities alg)
        (algebra-operations temp) (mapcar #'make-prod-op 
					  (algebra-operations alg)) )
       (when elems-p 
	   (setf (algebra-elem-list temp) (sg-alg gens temp) 
		 (algebra-elem-vector temp) 
		   (make-array (length (algebra-elem-list temp)) 
			       :initial-contents (algebra-elem-list temp)) )
	   (set-elem-ht (algebra-elem-list temp) temp) )
       temp ) )

(defun make-free-algebra-aux (n algebra
    &aux elems ops arities vector1 vector2 gens )
  (setq
    elems (algebra-elem-list algebra)
    ops (algebra-operations algebra)
    arities (algebra-arities algebra)
    vector1 (make-list n :initial-element elems)
    vector2 (make-list n :initial-element (car elems))
    gens (make-list n :initial-element (list (car elems))) )
  (loop
    (if (not (incq-vector2 vector1 vector2 elems)) (return gens))
    (setq n 0)
    (if
      (loop
        (if (homo-p (mapcar
              #'(lambda (x y) (cons (nth n x) y)) gens vector2)
            ops ops arities) (return nil))
        (if (eql (setq n (+ n 1)) (length (car gens))) (return t)) )
      (setq
        gens (mapcar #'cons vector2 gens)) ) ) )


;;; Below is the old stuff, not yet converted.

; algebra.lisp 7/13/88



;;; Words refer to expressions or terms in the language of the 
;;; algebra.  If  w  is a word in the variables vars, the 
;;; corresponding operation on  A  is (lambda (&rest vars) word).



;;;    * * * *    Utilities    * * * *

(defun genvector (n) ;; generate a vector of new symbols
 (cond
  ((zerop n) nil)
  (t (cons (gensym) (genvector (- n 1)))) ) )


;;;  * * * *   Testing Equations   * * * *



(defun test-id (ls rs vars elems
    &aux vector1 vector2)
  (setq
    ls (eval (list 'function (list 'lambda vars ls)))
    rs (eval (list 'function (list 'lambda vars rs)))
    vector1 (make-list (length vars) :initial-element elems)
    vector2 (make-list (length vars) :initial-element (car elems)) )
  (loop
   (cond
    ((not (equal (apply ls vector2) (apply rs vector2)))
      (setq *counter-example* vector2)
      (return nil))
    ((not (incq-vector2 vector1 vector2 elems)) (return t)) ) ) )

;;; The current values to be substituted are the cars of vector.
;;; This increments the vector by modifying it.
;;; The use of mapcar in test-id can be avoided by having two vectors,
;;; one like the present one and one with just the cars of the first.

(defun incq-vector2 (vector1 vector2 elems)
 (cond
  ((null vector1) nil)
  ((identity (cdar vector1))
    (rplaca vector1 (cdar vector1))
    (rplaca vector2 (caar vector1))
    't)
  (t
   (rplaca vector1 elems)
   (rplaca vector2 (car elems))
   (incq-vector2 (cdr vector1) (cdr vector2) elems) ) ) )


(defun eval-word (word varlist subs)
  (apply (list 'lambda varlist word) subs) )

;;;  * * * *   Polynomials   * * * *

(defun make-unary-poly (op pos constants var)
  (if (symbolp var) nil (setq var (gensym)))
  (list 'lambda (list var)
    (list 'apply (list 'quote op)
      (list 'insert-nth var (list 'quote constants) pos) )))



;;;  * * *  Normal subloops  * * *

;(defun nsbloop (gens elems ops arities
    ;&aux ans)
  ;(setq gens (remove-duplicates gens :test #'equal))
  ;(loop
    ;(setq
      ;ans gens
      ;gens (close1-list gens ops arities ans))
    ;(print 111111111111)
    ;(print (length gens))
    ;(if
     ;(equal gens ans) (return ans) ) ) )




; (defun make-prod-op (op arity
    ; &aux args)
  ; (list 'lambda (setq args (genvector arity))
         ; (list* 'mapcar (list 'quote op) args)))


(defun make-prod-op (op) 
  #'(lambda (&rest args) (mapcar #'(lambda (x) (apply op x)) 
				 (transpose args))) )
