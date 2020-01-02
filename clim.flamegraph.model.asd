(defsystem "clim.flamegraph.model"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                "let-plus"
                "utilities.print-items")

  :components  ((:module     "time"
                 :pathname   "src/time"
                 :serial     t
                 :components ((:file       "package")))

                (:module     "model"
                 :pathname   "src/model"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "util")
                              (:file       "mixins")
                              (:file       "standard-name") ; TODO remove "standard-" from filenames
                              (:file       "standard-function")
                              (:file       "standard-run")
                              (:file       "standard-tree")
                              (:file       "standard-region") ; TODO maybe
                              (:file       "standard-event") ; TODO maybe

                              (:file       "detach")))))
