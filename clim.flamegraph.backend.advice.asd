(defsystem "clim.flamegraph.backend.advice"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                "bordeaux-threads"
                "utilities.print-items"

                "clim.flamegraph.model")

  :components  ((:module     "backend-advice"
                 :pathname   "src/backend/advice"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "backend")))))
