

(setq *lat*
  (let ((gens '(a b c d e f ))
        (uc '(
              (a (d e))
              (b (d f))
              (c (e f))
              (d ())
              (e ())
              (f ())
           ))
        (meets '(
              (a ((d e)  ))
              (b ((d f)  ))
              (c ((e f)  ))
             ))
        (joins '(
              (d ((a b)  ))
              (e ((a c)  ))
              (f ((b c)  ))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )



