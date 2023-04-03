(defsystem "clim.flamegraph.application"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                "mcclim"
                "clouseau"

                "clim.flamegraph.model"
                "clim.flamegraph.recording"
                "clim.flamegraph.view"

		(:feature :sbcl "clim.flamegraph.backend.sb-sprof")
		(:feature :sbcl "clim.flamegraph.backend.sb-memory")
		(:feature :sbcl "clim.flamegraph.backend.advice"))

  :components  ((:module     "application"
                 :pathname   "src/application"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "source-configuration")

                              (:file       "presentations")
                              (:file       "commands")

                              (:file       "source-configuration-pane")
                              (:file       "recording")

                              (:file       "flamegraph-frame")
                              (:file       "application")))))
