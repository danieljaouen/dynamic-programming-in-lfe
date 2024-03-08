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

(defun lowest-cost-path-helper (g source sink)
  (if (== source sink)
    (progn #(ok [] 0))
    (let ((in-edges (digraph:in_edges g sink)))
      (if (== in-edges [])
        (tuple 'error "No path from source to sink")
        (my-min
         (lists:filter (lambda (x) (/= (element 1 x) 'error))
                       (lists:map
                        (lambda (edge)
                          (let (((tuple e v1 v2 cost-fn)
                                 (digraph:edge g edge)))
                            (progn
                              (case (lowest-cost-path-helper g source v1)
                                ((tuple 'ok reversed-path cost)
                                 (tuple 'ok `(,v1 ,@reversed-path) (+ cost (funcall cost-fn))))
                                ((tuple 'error msg)
                                 (tuple 'error msg))))))
                        in-edges)))))))

(defun lowest-cost-path (g source sink)
  (if (digraph_utils:is_acyclic g)
    (let ((result (lowest-cost-path-helper g source sink)))
      (case (element 1 result)
        ('ok (tuple 'ok `(,@(lists:reverse (element 2 result)) ,sink) (element 3 result)))
        ('error (tuple 'error "No path from source to sink"))))
    (tuple 'error "Graph is not acyclic")))
