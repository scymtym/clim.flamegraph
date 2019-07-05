(defsystem "clim.flamegraph.backend.sb-sprof"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:require "sb-sprof")

                "alexandria"

                "clim.flamegraph.model"
                "clim.flamegraph.recording"
                "clim.flamegraph.source")

  :components  ((:module     "backend-sbcl"
                 :pathname   "src/backend/sb-sprof"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "buffer")
                              (:file       "source")))))
