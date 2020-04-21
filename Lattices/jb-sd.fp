; file jb-sd.fp

; see his email of 4/1/2020

; Take the join closure of his relation and see if
; the resulting lattice is SD. 

; I know make-fp-lattice-from-relation only works in 
; some situations. So to be sure I made JB's relations 
; into a partial lattice. This means adding an 
; element g_{ki} for the right side of equation (k),
; k = 1,...,8. There are surprisingly many order relations
; between these new elements:
;
;    g_{5i} <= g_{1i} <= g_{2,i-1}
;    g_{1i} <= g_{7,i-1} 
;    g_{6i} <= g_{3i} <= g_{4,i-1}
;    g_{3i} <= g_{8,i-1} 
;    g_{5i} <= g_{2,i-2}
;    g_{6i} <= g_{4,i-2}
 


; Instructions:
; In the src directory do $ clisp -i bldnew.lisp
; Copy and paste the long 
; (setq *lat* ... (make-fp-lattice gens uc joins nil))
; below. Then do
; (close-under (generators)  #'s)
; or some subset of the generators like JB X_j's
; (make-finite-lattice-from-grtrql * #'grtrql)
; (setq jb  *)
; (sd-meet-p jb)
; (sd-join-p jb)
;
; kappa's of the ji's:
 (C0 B0) 
 (E0 (S G14 G21 G22 G34 G40 G41 G42 G71 G72 G80 G81 G82)) 
 (B0 (S G51 G80)) 
 (A1 (S D0 E0))
 (F0 (S G14 G20 G21 G22 G34 G41 G42 G70 G71 G72 G81 G82)) i
 (D0 (S G20 G61 G70 G71)) 
 (C1 (S F0 G70))
 (E1 (S G14 G22 G34 G40 G41 G42 G72 G80 G81 G82)) 
 (B1 (S G20 G40 G80 G81)) 
 (A2 (S G11 G80))
 (F1 (S G14 G20 G21 G22 G34 G42 G70 G71 G72 G82)) 
 (D1 (S G20 G21 G40 G70 G71 G72))
 (C2 (S G20 G31 G70 G71)) 
 (E2 (S G14 G34 G40 G41 G42 G80 G81 G82))
 (B2 (S G20 G21 G40 G41 G70 G80 G81 G82)) 
 (A3 (S G12 G20 G40 G70 G80 G81))
 (F2 (S G14 G20 G21 G22 G34 G70 G71 G72)) 
 (D2 (S G14 G20 G21 G22 G40 G41 G70 G71 G72 G80))
 (C3 (S G20 G21 G32 G40 G70 G71 G72 G80)) 
 (E3 (S E4 G34 G40 G41 G42 G80 G81 G82))
 (B3 (S G20 G21 G22 G34 G40 G41 G42 G70 G71 G80 G81 G82))
 (A4 (S G13 G20 G21 G40 G41 G70 G71 G80 G81 G82)) 
 (F3 (S F4 G14 G20 G21 G22 G70 G71 G72))
 (D3 (S G14 G20 G21 G22 G40 G41 G42 G70 G71 G72 G80 G81))
 (C4 (S G14 G20 G21 G22 G33 G40 G41 G70 G71 G72 G80 G81)) 
 (E4 (S G34 G40 G41 G42 G80 G81 G82))
 (F4 (S C4 G14 G20 G21 G22 G70 G71 G72))
;
;
;


(in-package :user)


(setq *lat*
  (let ((gens 
          '(a0 a1 a2 a3 a4 c0 c1 c2 c3 c4 b0 b1 b2 b3 d0 d1 d2 d3 e0 e1 e2 e3 e4 f0 f1 f2 f3 f4
            g11 g12 g13 g14 g20 g21 g22 g31 g32 g33 g34 g40 g41 g42 g51 g52 g53 g54
            g61 g62 g63 g64 g70 g71 g72 g80 g81 g82
                ))
        (uc '(
              (a0 (c0 e0 ))
              (a1 (c1 e1 g11))
              (a2 (c2 e2 g12 g20))
              (a3 (c3 e3 g13 g21))
              (a4 (c4 e4 g14 g22))
              (b0 (g11     g70 g20))
              (b1 (g12 g70 g71 g21))
              (b2 (g13 g71 g72 g22))
              (b3 (g14 g72  ))
              (c0 (a1 f0 ))
              (c1 (a2 f1 g31))
              (c2 (a3 f2 g32 g40))
              (c3 (a4 f3 g33 g41))
              (c4 (   f4 g34 g42))
              (d0 (g31     g80 g40))
              (d1 (g32 g80 g81 g41))
              (d2 (g33 g81 g82 g42))
              (d3 (g34 g82))
              (e0 (b0     g51))
              (e1 (b1 g51 g52))
              (e2 (b2 g52 g53))
              (e3 (b3 g53 g54))
              (e4 (   g54 ))
              (f0 (d0     g61 ))
              (f1 (d1 g61 g62 ))
              (f2 (d2 g62 g63 ))
              (f3 (d3 g63 g64 ))
              (f4 ( g64 ))
              (g11 (g20 g70 ))
              (g12 (g21 g71 ))
              (g13 (g22 g72 ))
              (g14 (     ))
              (g20 ( ))
              (g21 ( ))
              (g22 ( ))
              (g31 (g40 g80))
              (g32 (g41 g81))
              (g33 (g42 g82))
              (g34 ( ))
              (g40 ( ))
              (g41 ( ))
              (g42 ( ))
              (g51 (g11 ))
              (g52 (g12 g20 ))
              (g53 (g13 g21))
              (g54 (g14 g22))
              (g61 (g31 ))
              (g62 (g32 g40))
              (g63 (g33 g41))
              (g64 (g34 g42))
              ;(g64 (g34 g42))
              (g70 ( ))
              (g71 ( ))
              (g72 ( ))
              (g80 ( ))
              (g81 ( ))
              (g82 ( ))
             ))
        (meets nil)
        (joins '(
                  (g11 ((b0 c0)))  ; >= a1
                  (g12 ((b1 c1)))
                  (g13 ((b2 c2)))
                  (g14 ((b3 c3)))
                  (g20 ((e0 a2)))  ; >= b0
                  (g21 ((e1 a3)))
                  (g22 ((e2 a4)))
                  (g31 ((a1 d0)))  ; >= c1
                  (g32 ((a2 d1)))
                  (g33 ((a3 d2)))
                  (g34 ((a4 d3)))
                  (g40 ((f0 c2)))  ; >= d0
                  (g41 ((f1 c3)))
                  (g42 ((f2 c4)))
                  (g51 ((a1 e0)))  ; >= e1
                  (g52 ((a2 e1)))
                  (g53 ((a3 e2)))
                  (g54 ((a4 e3)))
                  (g61 ((c1 f0)))  ; >= f1
                  (g62 ((c2 f1)))
                  (g63 ((c3 f2)))
                  (g64 ((c4 f3)))
                  (g70 ((e0 b1)))  ; >= b0
                  (g71 ((e1 b2)))
                  (g72 ((e2 b3)))
                  (g80 ((f0 d1)))  ; >= d0
                  (g81 ((f1 d2)))
                  (g82 ((f2 d3)))
                )))
       (make-fp-lattice gens uc joins meets nil ) ) )

(defun lower-star (w &optional (lat *lat*) &aux (jis (lattice-ji-list lat))  )
  (join (remove w (ideal jis w lat) :test #'equal) lat) )

(defun kappa-fin-lattice (w &optional (lat *lat*) &aux (jis (lattice-ji-list lat)))
  (let* ((w* (lower-star w lat))
         (kw (remove-if
               #'(lambda (x) (lssql w (join (list w* x) lat) lat)) jis)) )
       (join kw lat)))

(defun upper-star (w &optional (lat *lat*) &aux (mis (lattice-mi-list lat))  )
  (join (remove w (filter mis w lat) :test #'equal) lat) )
              
(defun kappa-dual-fin-lattice (w &optional (lat *lat*) &aux (mis (lattice-mi-list lat)))
  (let* ((w* (upper-star w lat))
         (kw (remove-if
               #'(lambda (x) (grtrql w (meet (list w* x) lat) lat)) mis)) )
       (meet kw lat)))

(defun test (a b c &optional (lat *lat*))
  (and 
    (lssql a (join (list b c) lat) lat)
    (not (lssql a (join (list (lower-star b lat) c) lat) lat))
    (not (lssql a (join (list b (lower-star c lat)) lat) lat)) ) )
      
(and
(test 'a1 'b0 'c0 jb) 
(test 'a2 'b1 'c1 jb) 
(test 'a3 'b2 'c2 jb) 
(test 'a4 'b3 'c3 jb) 

(test 'b0 'e0 'a2 jb)
(test 'b1 'e1 'a3 jb)
(test 'b2 'e2 'a4 jb)

(test 'c1 'a1 'd0 jb)
(test 'c2 'a2 'd1 jb)
(test 'c3 'a3 'd2 jb)
(test 'c4 'a4 'd3 jb)

(test 'd0 'f0 'c2 jb)
(test 'd1 'f1 'c3 jb)
(test 'd2 'f2 'c4 jb)

(test 'e1 'a1 'e0 jb)
(test 'e2 'a2 'e1 jb)
(test 'e3 'a3 'e2 jb)
(test 'e4 'a4 'e3 jb)

(test 'f1 'c1 'f0 jb)
(test 'f2 'c2 'f1 jb)
(test 'f3 'c3 'f2 jb)
(test 'f4 'c4 'f3 jb)
)



;;; Old, dubious stuff


;(setq *lat* (make-fp-lattice-from-relations
;    '(a0 a1 a2 a3 a4 c0 c1 c2 c3 c4 b0 b1 b2 b3 d0 d1 d2 d3 e0 e1 e2 e3 e4 f0 f1 f2 f3)
;    '( 
;       (a0 c0)
;       (a0 e0)
;       (a1 c1)
;       (a1 e1)
;       (a2 c2)
;       (a2 e2)
;       (a3 c3)
;       (a3 e3)
;       (a4 c4)
;       (a4 e4)
;       (c0 a1)
;       (c0 f0)
;       (c1 a2)
;       (c1 f1)
;       (c2 a3)
;       (c2 f2)
;       (c3 a4)
;       (c3 f3)
;       (e0 b0)
;       (e1 b1)
;       (e2 b2)
;       (e3 b3)
;       (f0 d0)
;       (f1 d1)
;       (f2 d2)
;       (f3 d3)
;       (a1 (s b0 c0))
;       (a2 (s b1 c1))
;       (a3 (s b2 c2))
;       (a4 (s b3 c3))
;       (b0 (s e0 a2))
;       (b1 (s e1 a3))
;       (b2 (s e2 a4))
;       (c1 (s a1 d0))
;       (c2 (s a2 d1))
;       (c3 (s a3 d2))
;       (c4 (s a4 d3))
;       (d0 (s f0 c2))
;       (d1 (s f1 c3))
;       (e1 (s a1 e0))
;       (e2 (s a2 e1))
;       (e3 (s a3 e2))
;       (e4 (s a4 e3))
;       (f1 (s c1 f0))
;       (f2 (s c2 f1))
;       (f3 (s c3 f2))
;       (b0 (s e0 b1))
;       (b1 (s e1 b2))
;       (b2 (s e2 b3))
;       (d0 (s f0 d1))
;       (d1 (s f1 d2))
;       (d2 (s f2 d3))
;      )))
;
;(setq *lat* (make-fp-lattice-from-relations
;    '(a0 a1 a2 a3 a4 c0 c1 c2 c3 c4 b0 b1 b2 b3 d0 d1 d2 d3 e0 e1 e2 e3 e4 f0 f1 f2 f3)
;    '( 
;       (a0 c0)
;       (a0 e0)
;       (a1 c1)
;       (a1 e1)
;       (a2 c2)
;       (a2 e2)
;       (a3 c3)
;       (a3 e3)
;       (a4 c4)
;       (a4 e4)
;       (c0 a1)
;       (c0 f0)
;       (c1 a2)
;       (c1 f1)
;       (c2 a3)
;       (c2 f2)
;       (c3 a4)
;       (c3 f3)
;       (e0 b0)
;       (e1 b1)
;       (e2 b2)
;       (e3 b3)
;       (f0 d0)
;       (f1 d1)
;       (f2 d2)
;       (f3 d3)
;       (a1 (s b0 c0))
;       (a2 (s b1 c1))
;       (a3 (s b2 c2))
;       (a4 (s b3 c3))
;       (b0 (s e0 a2))
;       (b1 (s e1 a3))
;       (b2 (s e2 a4))
;       (c1 (s a1 d0))
;       (c2 (s a2 d1))
;       (c3 (s a3 d2))
;       (c4 (s a4 d3))
;       (d0 (s f0 c2))
;       (d1 (s f1 c3))
;       (e1 (s a1 e0))
;       (e2 (s a2 e1))
;       (e3 (s a3 e2))
;       (e4 (s a4 e3))
;       (f1 (s c1 f0))
;       (f2 (s c2 f1))
;       (f3 (s c3 f2))
;       (b0 (s e0 b1))
;       (b1 (s e1 b2))
;       (b2 (s e2 b3))
;       (d0 (s f0 d1))
;       (d1 (s f1 d2))
;       (d2 (s f2 d3))
;      )))
;
;
;;;;;;;;;;;;;; delete me ;;;;;;;;;
;(let* ((gens '(a b c z))
;       (uc '(
;           (a ())
;           (b ())
;           (c ())
;	   (z (a b c))))
;       (joins '())
;       (meets '(
;	      (z ((a b) (a c) (b c))))))
;      (setq atomless (make-fp-lattice gens uc joins meets t)))
;
;(setq *lat* atomless)
;
;
;(let* ((gens '(a b c z u))
;       (uc '(
;           (u ())
;           (a (u))
;           (b (u))
;           (c (u))
;           (z (a b c))))
;       (joins '() )
;       (meets '(
;              (z ((a b) (a c) (b c))))) )
;      (setq atomless0 (make-fp-lattice gens uc joins meets t)))
;
;(let* ((gens '(a b c z u))
;       (uc '(
;           (u ())
;           (a (u))
;           (b (u))
;           (c (u))
;           (z (a b c))))
;       (joins '(
;              (u ((a b)))))
;       (meets '(
;              (z ((a b) (a c) (b c))))) )
;      (setq atomless1 (make-fp-lattice gens uc joins meets t)))
;
;(let* ((gens '(a b c z u))
;       (uc '(
;           (u ())
;           (a (u))
;           (b (u))
;           (c (u))
;           (z (a b c))))
;       (joins '(
;              (u ((a b) (a c)   ))))
;       (meets '(
;              (z ((a b) (a c) (b c))))) )
;      (setq atomless2 (make-fp-lattice gens uc joins meets t)))
;
;(let* ((gens '(a b c z u))
;       (uc '(
;           (u ())
;           (a (u))
;           (b (u))
;           (c (u))
;           (z (a b c))))
;       (joins '(
;              (u ((a b) (a c) (b c)   ))))
;       (meets '(
;              (z ((a b) (a c) (b c))))) )
;      (setq atomless3 (make-fp-lattice gens uc joins meets t)))






