(defsystem "clim.flamegraph.sbcl-defaults"
  :version    (:read-file-from "version-string.sexp")
  :depends-on ("clim.flamegraph.backend.advice"
               "clim.flamegraph.backend.sb-memory"
               "clim.flamegraph.backend.sb-sprof"

               "clim.flamegraph.application"))
