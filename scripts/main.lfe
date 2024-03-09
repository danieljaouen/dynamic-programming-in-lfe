#! /usr/bin/env lfescript

;;; --------------------
;;; entry point function
;;; --------------------

(defun main (args)
  (let* ((g (digraph:new))
         (source (digraph:add_vertex g 'source))
         (sink (digraph:add_vertex g 'sink))
         (v1 (digraph:add_vertex g 'v1))
         (v2 (digraph:add_vertex g 'v2))
         (v3 (digraph:add_vertex g 'v3))
         (e1 (digraph:add_edge g source v1 (dynamic-programming-in-lfe:make-cost-function 1)))
         (e2 (digraph:add_edge g source v2 (dynamic-programming-in-lfe:make-cost-function 2)))
         (e3 (digraph:add_edge g v1 sink (dynamic-programming-in-lfe:make-cost-function 3)))
         (e4 (digraph:add_edge g v2 sink (dynamic-programming-in-lfe:make-cost-function 4)))
         (e5 (digraph:add_edge g v3 sink (dynamic-programming-in-lfe:make-cost-function 1))))
    (case (dynamic-programming-in-lfe:lowest-cost-path g source sink)
      ((tuple 'ok path cost) (lfe_io:format "Path: ~p~nCost: ~p~n" `(,path ,cost)))
      ((tuple 'error msg) (lfe_io:format msg '())))))
