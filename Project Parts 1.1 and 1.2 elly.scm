

;;vertex constructor
(define (make-vertex contents) contents)

;vertexList constructor/selector


;takes: vertex , post: (vertex)
(define (make-vertexList vertex) (list vertex))
(define (firstVertexInV-List vertexList)(car vertexList))
(define (restVertexList vertexList)(cdr vertexList))
;add vertex to vertex list, check for repeats before adding
;;post: (...vertex...)
(define (addVertexToVertexList vertexList vertex)
  
  (cond((null? vertexList)(make-vertexList vertex))

       ((equal? (firstVertexInV-List vertexList) vertex) vertexList)

       (else (cons (firstVertexInV-List vertexList)(addVertexToVertexList (restVertexList vertexList) vertex)))))

;;edges constructors/selectors 

;;takes 2 vertices , post (vertex , vertex)
(define (makeEdge From to)(cons From (list To)))

(define (From edge)(car edge))

(define (To edge)(cadr edge))

;;edgeList constructors/ selectors

(define(makeEmptyEdgeList) '())
;;add edge to edgelist or make new list, repeats allowed
(define (addEdgeToEdgeList edge edgeList)

  (cond ((null? edgeList)(list edge))

        (else (cons edge edgeList))))

;adjListblock constructor/ selectors

;takes: index vertexList , post (index vertexList)
(define (makeAdjListBlock index vertexList )
(list index vertexList) )
;takes adjlistblock, returns (adjlistblock)
(define (Index adjListBlock)(car adjListBlock))
(define (vertexList adjListBlock)(cadr adjListBlock))

;;adjList constructors/ selectors
(define (makeAdjList adjListBlock)(list adjListBlock))
(define (firstBlockInAdjList adjList) (car adjList))
(define (restOfAdjList adjList) (cdr adjList))


;;post ((indexselector ( vertexselector)))
(define (addEdgeToAdjList edge adjList IndexSelector VertexSelector) 
  ;is adjList empty? that means our inputted index hasnt been created. Create it.
  (cond((null? adjList) (makeAdjList (makeAdjListBlock (IndexSelector edge) (addVertexToVertexList '()(VertexSelector edge)))))
       ;is the indexselector of edge the same as index in first adjblock?
       ;make a new adjblock with the edge's vertexselector added in, cons it into restofAdjlist
       ((equal? (IndexSelector edge)(Index (firstBlockInAdjList adjList))) (cons(makeAdjListBlock (IndexSelector edge) (addVertexToVertexList (vertexList(firstBlockInAdjList adjList)) (VertexSelector edge) )) (restOfAdjList adjList)))
       ;
       (else (cons(firstBlockInAdjList adjList)(addEdgeToAdjList edge (restOfAdjList adjList)IndexSelector VertexSelector)))))



;;classic accumilate from the notes
(define (accumulate op init seq)
  (cond((null? seq)init)
       (else(op (car seq)(accumulate op init (cdr seq))))))

;;just accumilate addEdgeToAdjList over edgeList to get directed graph
(define(makeDirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y From To)) '() edgeList ))
;;to get undirected, accumilate addEdgeToAdjList over edgeList but flip the index and vertex selector functions. Also have the directed version be the init value in accumilate so-
;the accumilate call is this: (accumulate addEdgeToAdjList DirectedAdjList edgeList)
(define(makeUndirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y To From))(makeDirectedAdjList edgeList )edgeList))

(define myEdgeList '((a b)(b a)(b 1)(b 2)(1 2)))

(define BuiltEdgeList (addEdgeToEdgeList(makeEdge (make-vertex 'a) (make-vertex 'b)) (makeEmptyEdgeList)))
(define BuiltAdjList (makeUndirectedAdjList BuiltEdgeList))
