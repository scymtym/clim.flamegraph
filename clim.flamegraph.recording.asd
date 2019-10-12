(defsystem "clim.flamegraph.recording"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                "bordeaux-threads"
                "lparallel")

  :components  ((:module     "recording"
                 :pathname   "src/recording"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "run-builder")
                              (:file       "recorder")
                              (:file       "macros")))))
