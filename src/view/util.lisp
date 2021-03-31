;;;; util.lisp --- Utilities used in the view module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view)

(defun truncate-string (string length)
  (if (>= (length string) length)
      (let ((half (floor length 2)))
        (concatenate 'string
                     (subseq string 0 half)
                     "…"
                     (subseq string (- (length string) half))))
      string))

(defun single-line (string)
  (substitute #\¶ #\Newline string))
