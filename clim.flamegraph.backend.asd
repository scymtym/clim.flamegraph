(defsystem "clim.flamegraph.backend"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria")

  :components  ((:module     "backend"
                 :pathname   "src/backend"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "mixins")))))
