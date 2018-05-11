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

  :components  ())
