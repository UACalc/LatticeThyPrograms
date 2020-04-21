; latstruct.lisp   1/1/89

;;; 11/30/93 I changed (lattice-hash-table lat) to use an #'eq test
;;; 	     since it entries are now hash tables.

;;; This uses lisp structures for lattices.

;;;(in-package :user)
;(export '(*lat* lattice ))

(defvar *lat* nil)    ; the default lattice

(defstruct (lattice 
	    (:constructor create-lattice)
	    (:print-function 
	     (lambda (lat stream depth)
		     (declare (ignore depth))
		     (if *print-pretty* 
			 (print-lattice lat stream)
			 (print-lattice-short lat stream)))))
  type          ; free, finitely presented, finite, congruence, etc
  lock          ;; for the new multitasking
  elem-list     ;; a list of elements
  elem-p        ;; recognizer f
  symbol-names  ;  the names of the symbols used to make P from relations
  lssql         ;; fn
  grtrql        ;  fn
  hash-table	;  for use by lssql
  upper-covers-list
  lower-covers-list
  filters       ; an alist
  ideals        ; an alist
  cover-p       ;; (cover-p a b) is T if  b  covers  a.
  upper-covers  ;; returns a list of the upper covers of its argument
  lower-covers
  zero          ;; an elem
  one
  generators    ;; list
  ji-list       ;; list of the join irreducibles
  mi-list       ;; list of the meet irreducibles
  join-covers-list
  join-covers   ; a fn
  meet-covers-list
  meet-covers
  dag-list      ;; alist of the daggers of the ji generators
  ddag-list     ;; alist of the dual daggers of the mi generators
  j-list        ;; list of (p  J(p))
  j             ;  a fn
  j-dual-list   ;; list of (p  J^dual(p))
  j-dual        ;  a fn
  beta_0        ;; ???
  alpha_0       ;; ???
  kappa-list    ;  kappa of the generators
  kappa-dual-list
  ;; need to have this get evaluated only when needed
  ;; end fp stuff
  join          ;; fn
  meet
  join-table    ;; a list (or hash-table)
  meet-table
  ji-p          ;; recognizer fn
  mi-p
  cji-p
  cmi-p
  con		;; the congruence lattice
  (pprint-elem 'identity)
  hasse-diagram ;; a structure
  )

;;; fancy lattice printing

(defun print-lattice-short (lat stream)
  (let ((kind (lattice-type lat)))
       (if (member kind '(congruence finitely-presented))
 	   (format stream "#<~a lattice>" 
	     (string-downcase (symbol-name kind)))
 	   (format stream "#<~a>" 
	     (string-downcase (symbol-name kind))))))

(defun print-lattice (lat &optional (stream t))
  (format stream "lattice:~%")
  (format stream "~t type:~15t ~a~%"
          (string-downcase (symbol-name (lattice-type lat))))
  (if (lattice-elem-list lat)
      (progn
       (format stream "~t elements:~15t ~s~%" (lattice-elem-list lat))
       (format stream "~t size:~15t ~d~%" (length (lattice-elem-list lat)))))
  (if (lattice-generators lat)
    (format stream "~t generators:~15t ~s~%" (lattice-generators lat)))
  (if (lattice-symbol-names lat)
    (format stream "~t symbol-names:~15t ~s~%" (lattice-symbol-names lat)))
  (if (lattice-ji-list lat)
    (format stream "~t join irreds:~15t ~s~%" (lattice-ji-list lat)))
  (if (lattice-mi-list lat)
    (format stream "~t meet irreds:~15t ~s~%" (lattice-mi-list lat)))
  (if (lattice-join-covers-list lat)
    (format stream "~t join covers:~15t ~s~%"
            (lattice-join-covers-list lat)))
  (if (lattice-meet-covers-list lat)
    (format stream "~t meet covers:~15t ~s~%"
            (lattice-meet-covers-list lat)))
  (if (lattice-con lat)
      (progn
       (format stream "~t con:~%")
       (if (lattice-elem-list (con lat))
           (format stream "~5t size:~15t ~d~%" (card (con lat))) )
           (format stream "~5t no. ji members:~15t ~d~%"
            (length (lattice-ji-list (con lat)))) ) ) )


;;; this is not implemented yet since it would change the format
;(defmacro make-lat-fn (fn-name slot-name)
  ;;  slot-name is a alist like lattice-j-list
  ;((symbolp fn-name)
    ;`(defun ,fn-name (elt lat)
      ;(second (assoc elt (,slot-name lat) )) ) ) )

;(defmacro set-lat ()



(defun lssql (v u &optional (lat *lat*))
  (if (lattice-p lat)
    (funcall (lattice-lssql lat) v u)
    (error "No current lattice") ) )

(defun grtrql (v u &optional (lat *lat*))
  (if (lattice-p lat)
    (funcall (lattice-grtrql lat) v u)
    (error "No current lattice") ) )

(defun join (elem-list &optional (lat *lat*))
  (if (lattice-p lat)
    (funcall (lattice-join lat) elem-list)
    (error "No current lattice") ) )

(defun meet (elem-list &optional (lat *lat*))
  (if (lattice-p lat)
    (funcall (lattice-meet lat) elem-list)
    (error "No current lattice") ) )

;;; 11/90 adding a check to see that the arguments to  s  and  p  are
;;; really in the lattice. The variable *check-elems* can be used to
;;; prevent checking in things like close-under.

(defvar *check-elems* t)

;;; Since the lattice is not specified, s should not be used in programs
(defun s (&rest args)
  (if (lattice-p *lat*)
      (progn
       (if (lattice-elem-p *lat*)
           (let ((args2 args))
                (loop
	         (if (null args2) (return))
	         (unless (funcall (lattice-elem-p *lat*) (car args2))
		     (error "s: ~S is not in the current lattice." 
			    (car args2)) )
	         (pop args2))) )
       (funcall (lattice-join *lat*) args) )
       (error "No current lattice") ) )

;;; Since the lattice is not specified, p should not be used in programs
(defun p (&rest args)
  (if (lattice-p *lat*)
      (progn
       (if (lattice-elem-p *lat*)
           (let ((args2 args))
                (loop
	         (if (null args2) (return))
	         (unless (funcall (lattice-elem-p *lat*) (car args2))
		     (error "s: ~S is not in the current lattice." 
			    (car args2)) )
	         (pop args2))) )
       (funcall (lattice-meet *lat*) args))
       (error "No current lattice") ) )



;;;       * * *  Drawing stuff * * *


;(defstruct (hasse-diagram (:constructor create-hasse-diagram))
;  name                                  ; print name
;  (lattice (if (boundp '*lat*) *lat*))  ; the lattice structure
;  ;; these next four may not be the right thing
;  ;; maybe something like highest, most-right and left position
;  ;; would be better
;  (width *hasse-diagram-width*)
;  (height *hasse-diagram-height*)
;  (x-scaling *x-scaling*)
;  (y-scaling *y-scaling*)
;  elements                              ; a list
;  positions                             ; a list
;  print-names                           ; a list
;  lower-covers                          ;    "
;  upper-covers                          ;    "
;;;  )
  ;; we might want just list instead of a-list because user input
  ;; lattices, for example, may not have any names for the elements.
  ;; Alternately, we could have artificial names and use print-names
  ;; if the user wants to input a name.

;(defparameter *hasse-diagram-height* 10)
;(defparameter *hasse-diagram-width* 10)
;(defparameter *x-scaling* 30)
;(defparameter *y-scaling* 30)

;;; we also need thing that make a hasse-diagram from a list of
;;; elements and the order relation or a list of upper-covers.


;;; ####################################################
;;; ##########  Saving a lattice to a file  ############
;;; ####################################################

(defun save-lattice (name file &optional (lat *lat*))
  (if (not (symbolp name))
      (error "The first argument should be a symbol, ~% ~
	     the second a string for the file name."))
  (with-open-file (ofile file :direction :output)
    (format ofile ";;; automatically saved lattice~%~%")
    (format ofile "(in-package \"USER\")~%~%")
    (format ofile "(setq ~A (create-lattice))~%~%" name )
    (format ofile "(setf (lattice-type ~A) '~S)~%" name (lattice-type lat))
    (format ofile "(setf (lattice-elem-list ~A) '~S)~%"
      name (lattice-elem-list lat))
    (format ofile "(setf (lattice-upper-covers-list ~A) '~S)~%"
      name (lattice-upper-covers-list lat))
    (format ofile "(setf (lattice-lower-covers-list ~A) '~S)~%"
      name (lattice-lower-covers-list lat))
    (format ofile "(setf (lattice-filters ~A) '~S)~%"
      name  (lattice-filters lat))
    (format ofile "(setf (lattice-ideals ~A) '~S)~%"
      name  (lattice-ideals lat))
    (format ofile "(setf (lattice-join-table ~A) '~S)~%"
      name  (lattice-join-table lat))
    (format ofile "(setf (lattice-meet-table ~A) '~S)~%"
      name  (lattice-meet-table lat))
    (format ofile "(setf (lattice-zero ~A) '~S)~%"
      name  (lattice-zero lat))
    (format ofile "(setf (lattice-one ~A) '~S)~%"
      name  (lattice-one lat))
    (format ofile "(setf (lattice-generators ~A) '~S)~%"
      name  (lattice-generators lat))
    (format ofile "(setf (lattice-ji-list ~A) '~S)~%"
      name  (lattice-ji-list lat))
    (format ofile "(setf (lattice-mi-list ~A) '~S)~%"
      name  (lattice-mi-list lat))
    (format ofile "(setf (lattice-join-covers-list ~A) '~S)~%"
      name  (lattice-join-covers-list lat))
    (format ofile "(setf (lattice-meet-covers-list ~A) '~S)~%"
      name  (lattice-meet-covers-list lat))
    (format ofile "(setf (lattice-dag-list ~A) '~S)~%"
      name  (lattice-dag-list lat))
    (format ofile "(setf (lattice-ddag-list ~A) '~S)~%"
      name  (lattice-ddag-list lat))
    (format ofile "(setf (lattice-j-list ~A) '~S)~%"
      name  (lattice-j-list lat))
    (format ofile "(setf (lattice-j-dual-list ~A) '~S)~%"
      name  (lattice-j-dual-list lat))
    (format ofile "(setf (lattice-kappa-list ~A) '~S)~%"
      name  (lattice-kappa-list lat))
    (format ofile "(setf (lattice-kappa-dual-list ~A) '~S)~%"
      name  (lattice-kappa-dual-list lat))
    (format ofile "(setf (lattice-pprint-elem ~A) '~S)~%"
      name  (lattice-pprint-elem lat))
    (format ofile "(setf (lattice-hasse-diagram ~A) '~S)~%"
      name  (lattice-hasse-diagram lat))
    (format ofile "~%~%(restore-lattice-fns ~A)~%" name) ))

(defun restore-lattice-fns (lat)
  (case (lattice-type lat)
    (finite-lattice (restore-fns-fin lat))
    (finitely-presented (restore-fns-fp lat))
    (t (error
        "restore-lattice-fns not yet implimented for lattice type ~A"
        (lattice-type lat) )) ) )

(defun restore-fns-fin (lat)
  (setf (lattice-lssql lat)
    #'(lambda (x y)
        (MEMBER Y (SECOND
         (ASSOC X (lattice-filters lat) :test #'equal)) :test #'equal)))
  (setf (lattice-grtrql lat)
    #'(lambda (x y)
        (MEMBER x (SECOND
         (ASSOC y (lattice-filters lat) :test #'equal)) :test #'equal)))
  (setf (lattice-join lat)
        #'(lambda (args)
                  (reduce
                   #'(lambda (x y)
                             (second
                              (assoc
                               y (cdr (assoc x (lattice-join-table lat)
                                             :test #'equal)) :test #'equal)))
                   args)) )
  (setf (lattice-meet lat)
        #'(lambda (args)
                  (reduce
                   #'(lambda (x y)
                             (second
                              (assoc
                               y (cdr (assoc x (lattice-meet-table lat)
                                             :test #'equal)) :test #'equal)))
                   args)) )
  (setf (lattice-upper-covers lat)
    #'(lambda (x) (second (assoc x (lattice-upper-covers-list lat)
          :test #'equal))))
  (if (lattice-lower-covers-list lat)
    (setf (lattice-lower-covers lat)
      #'(lambda (x) (second (assoc x (lattice-lower-covers-list lat)
            :test #'equal)))))
  (if (lattice-join-covers-list lat)
    (setf (lattice-join-covers lat)
      #'(lambda (x) (second (assoc x (lattice-join-covers-list lat)
            :test #'equal)))))
  (if (lattice-meet-covers-list lat)
    (setf (lattice-meet-covers lat)
      #'(lambda (x) (second (assoc x (lattice-meet-covers-list lat)
            :test #'equal)))))
  (if (lattice-j-list lat)
    (setf (lattice-j lat)
      #'(lambda (x) (second (assoc x (lattice-j-list lat)
            :test #'equal)))))
  (if (lattice-j-dual-list lat)
    (setf (lattice-j-dual lat)
      #'(lambda (x) (second (assoc x (lattice-j-dual-list lat)
            :test #'equal))))) )

(defun restore-fns-fp (lat)
  (setf (lattice-lssql lat)
    #'(lambda (x y) (lssql-fp x y lat)))
  (setf (lattice-grtrql lat)
    #'(lambda (x y) (lssql-fp  y x lat)))   ;; hash changed to equal 11/90
  (setf (lattice-hash-table lat) (make-hash-table :test #'eq) )
  (setf (lattice-join lat) #'(lambda (list) (join-fp list lat)))
  (setf (lattice-meet lat) #'(lambda (list) (meet-fp list lat)))
  (if (lattice-join-covers-list lat)
    (setf (lattice-join-covers lat)
      #'(lambda (x) (second (assoc x (lattice-join-covers-list lat)
            :test #'equal)))))
  (if (lattice-meet-covers-list lat)
    (setf (lattice-meet-covers lat)
      #'(lambda (x) (second (assoc x (lattice-meet-covers-list lat)
            :test #'equal)))))
  (if (lattice-j-list lat)
    (setf (lattice-j lat)
      #'(lambda (x) (second (assoc x (lattice-j-list lat)
            :test #'equal)))))
  (if (lattice-j-dual-list lat)
    (setf (lattice-j-dual lat)
      #'(lambda (x) (second (assoc x (lattice-j-dual-list lat)
            :test #'equal))))) )

