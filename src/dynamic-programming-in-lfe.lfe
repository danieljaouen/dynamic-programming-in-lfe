(defmodule dynamic-programming-in-lfe
  (export
   (lowest-cost-path 3)
   (make-cost-function 1)))

(include-lib "lfe/include/clj.lfe")

;;; -----------
;;; library API
;;; -----------

(defun make-cost-function (n) (lambda () n))

(defun my-min (lst)
  (if (== lst [])
    #(error "Empty list passed to my-min")
    (lists:foldl
     (lambda (x y)
       (if (< (element 3 x) (element 3 y))
         x
         y))
     (hd lst)
     (tl lst))))

(defun calculate-current-path (g source edge)
  (let (((tuple e v1 v2 cost-fn)
         (digraph:edge g edge)))
    (case (lowest-cost-path-helper g source v1)
      ((tuple 'ok path cost)
       (tuple 'ok `(,@path ,v2) (+ cost (funcall cost-fn))))
      ((tuple 'error msg)
       (tuple 'error msg)))))

(defun lowest-cost-path-helper (g source sink)
  (if (== source sink)
    (tuple 'ok (list source) 0)
    (let ((in-edges (digraph:in_edges g sink)))
      (if (== in-edges [])
        (tuple 'error "Cannot reach source from current path")
        (clj:->> in-edges
                 (lists:map (lambda (edge) (calculate-current-path g source edge)))
                 (lists:filter (lambda (x) (== (element 1 x) 'ok)))
                 (my-min))))))

(defun lowest-cost-path (g source sink)
  (if (not (digraph_utils:is_acyclic g))
    (tuple 'error "Graph is not acyclic")
    (let ((result (lowest-cost-path-helper g source sink)))
      (case result
        ((tuple 'ok path cost)
         (tuple 'ok path cost))
        ((tuple 'error msg)
         (tuple 'error "No path from source to sink"))))))
