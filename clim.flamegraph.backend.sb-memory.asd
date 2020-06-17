(defsystem "clim.flamegraph.backend.sb-memory"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                "clim.flamegraph.model"
                "clim.flamegraph.recording"
                "clim.flamegraph.backend")

  :components  ((:module     "backend-sb-memory"
                 :pathname   "src/backend/sb-memory"
                 :serial     t
                 :components ((:file       "package")))))
