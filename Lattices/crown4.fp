

(setq *lat*
  (let ((gens '(a b c d e f g h ))
        (uc '(
              (a (e f))
              (b (f g))
              (c (g h))
              (d (h e))
              (e ())
              (f ())
              (g ())
              (h ())
           ))
        (meets '(
              (a ((e f)  ))
              (b ((f g)  ))
              (c ((g h)  ))
              (d ((h e)  ))
             ))
        (joins '(
              (e ((a d)  ))
              (f ((a b)  ))
              (g ((b c)  ))
              (h ((c d)  ))
             )))
    (make-fp-lattice gens uc joins meets t ) ) )



