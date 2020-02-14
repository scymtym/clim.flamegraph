(defsystem "clim.flamegraph.backend.mezzano-profiler"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                "clim.flamegraph.model"
                "clim.flamegraph.recording"
                "clim.flamegraph.source")

  :components  ((:module     "mezzano-profiler"
                 :pathname   "src/backend/mezzano-profiler"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "buffer")
                              (:file       "source")))))
