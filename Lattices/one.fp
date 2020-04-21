; one.fp


;;; This can be defined with one relation: a \meet b = a \meet c.
;;; It fails SD_\meet.
;;; It seems likely that a(b + c)/0 has no covers. There are 6 elements
;;; in PSPS({a, b, c}) in this interval; none has a lower cover and
;;; none has an upper cover in the interval.


(in-package :user)


(setq *lat*
  (let ((gens '(a b c 0))
        (uc '(
              (0 (a b c))
              (b ())
              (a ())
              (c ())
           ))
        (meets '(
              (0 ((a b) (a c)))
             ))
        (joins '(
             )))
    (make-fp-lattice gens uc joins meets t ) )
  one *lat*)

