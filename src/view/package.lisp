(cl:defpackage #:clim.flamegraph.view
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:model #:clim.flamegraph.model))

  (:export
   #:package-color
   #:symbol-color))
