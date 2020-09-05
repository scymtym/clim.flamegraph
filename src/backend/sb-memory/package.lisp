;;;; package.lisp --- Package definition for the sb-memory backend.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.backend.sb-memory
  (:use
   #:cl)

  (:local-nicknames
   (#:a      #:alexandria)

   (#:time   #:clim.flamegraph.time)
   (#:model  #:clim.flamegraph.model)

   (#:record #:clim.flamegraph.recording))

  (:export
   #:source))
