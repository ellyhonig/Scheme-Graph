
;(load "vertex.rkt")
;(load "edges.rkt")
;;test structures
;(define myEdgeList '((a b)(b a)(b 1)(b 2)(1 2)))
;(define myAdjList (makeUndirectedAdjList myEdgeList))

;(define BuiltEdgeList (addEdgeToEdgeList(makeEdge (make-vertex 'a) (make-vertex 'b)) (makeEmptyEdgeList)))
;(define BuiltAdjList (makeUndirectedAdjList BuiltEdgeList))


(define myGraph '((1 2 3 4)((1 2)(2 3)(2 4))))


(define myGraph2
'((A B C D E)((A B)
(B D)
(A C)
(B E)
(C E)
(D E))))