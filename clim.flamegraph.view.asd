(defsystem "clim.flamegraph.view"
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                "text.orders-of-magnitude"

                "mcclim"

                "clim.flamegraph.model")

  :components  ((:module     "view"
                 :pathname   "src/view"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "colors")
                              (:file       "presentations")
                              (:file       "highlighting")))

                (:module     "region"
                 :depends-on ("view")
                 :pathname   "src/view/region"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "presentations")))

                (:module     "timeline"
                 :depends-on ("view" "region") ; TODO region is temp
                 :pathname   "src/view/timeline"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "model")
                              (:file       "presentations")
                              (:file       "timeline-pane")
                              (:file       "pane")))

                (:module     "flat"
                 :depends-on ("view")
                 :pathname   "src/view/flat"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "presentations")
                              (:file       "pane")))

                (:module     "flamegraph"
                 :depends-on ("view")
                 :pathname   "src/view/flamegraph"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "presentations")
                              (:file       "pane")))))
