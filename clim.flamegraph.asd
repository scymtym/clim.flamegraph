;;;; clim.flamegraph.asd --- System definition for the clim.flamegraph system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "clim.flamegraph"
  :description "Flamegraph-style visualization of sb-sprof results in CLIM."
  :license     "GPLv3"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("sb-sprof"

                "alexandria"

                "mcclim")

  :components  ((:module     "src"
                 :serial     t
                 :components ((:file "package")

                              ;; Data structures
                              (:file "trace-tree")
                              (:file "model")

                              ;; Application
                              (:file "application")

                              ;; Presentations and views
                              (:file "colors")
                              (:file "timeline")

                              (:file "flat")
                              (:file "flamegraph")

                              ;; Commands
                              (:file "commands")

                              ;;
                              (:file "interface"))))

  :perform (load-op :before (operation component)
             (sb-ext:assert-version->= 1 4 6)))
