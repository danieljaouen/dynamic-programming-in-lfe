(defmodule dynamic-programming-in-lfe
  (export
   (lowest-cost-path 3)
   (make-cost-function 1)))

;;; -----------
;;; library API
;;; -----------

(defun make-cost-function (n)
  (lambda ()
    n))

(defun lowest-cost-path (g source sink)
  'hello-world)
