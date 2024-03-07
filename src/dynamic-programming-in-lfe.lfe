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
  (let ((in-edges (digraph:in_edges g sink)))
    (if (== in-edges [])
      (if (== source sink)
        #(ok 0)
        #(error "No path from source to sink")))
    (lists:min
     (lists:map (lambda (edge_with_cost)
                  (let ((e (car edge_with_cost))
                        (cost (cdr edge_with_cost)))
                    (progn
                      (lfe_io:format "edge: ~p~n" `(,e))
                      (lfe_io:format "cost: ~p~n" `(,cost))
                      (lfe_io:format "~n~n" '())
                      5)))
                in-edges))))
