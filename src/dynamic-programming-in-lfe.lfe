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
  (if (== source sink)
    (progn #(ok 0))
    (let ((in-edges (digraph:in_edges g sink)))
      (if (== in-edges [])
        #(error "No path from source to sink")
        (lists:min
         (lists:filter (lambda (x) (/= (element 1 x) 'error))
                       (lists:map
                        (lambda (edge)
                          (let (((tuple e v1 v2 cost-fn)
                                 (digraph:edge g edge)))
                            (progn
                              (case (lowest-cost-path g source v1)
                                ((tuple 'ok cost)
                                 (tuple 'ok (+ cost (funcall cost-fn))))
                                ((tuple 'error msg)
                                 #(error msg))))))
                        in-edges)))))))
