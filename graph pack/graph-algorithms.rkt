(load "types.rkt")
(load "examples.rkt")

(define (BFS adjList VisitedStack queue) 
   (cond((null? queue))
        (else(BFS adjList (push (frontOfQueue queue) VisitedStack) (accumulate (lambda(x y)(enqueue x y)) (dequeue queue) (vertexList-ofAdjList(adjListBlockOfIndex adjList (frontOfQueue queue)) ) ) ))))