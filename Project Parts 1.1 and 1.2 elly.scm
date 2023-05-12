


;;edges constructors/selectors 
(define (makeEdge From to)(cons From (list To)))

(define (From edge)(car edge))

(define (To edge)(cadr edge))

;;edgeList constructors/ selectors

(define(makeEdgeList) '())
;;add edge to edgelist or make new list, repeats allowed
(define (addEdge edge edgeList)

  (cond ((null? edgeList)(list edge))

        (else (cons edge edgeList))))

;adjListblock constructor/ selectors
(define (makeAdjListBlock index vertexList )
(list index vertexList) )

(define (Index adjListBlock)(car adjListBlock))
(define (vertexList adjListBlock)(cadr adjListBlock))

(define (firstBlockInAdjList adjList) (car adjList))
(define (restOfAdjList adjList) (cdr adjList))

;add vertex to vertex list within adjList block, check for repeats before adding
;;post: (...vertex...)
(define (addVertexToIndex arry vertex)
  
  (cond((null? arry)(list vertex))

       ((equal? (car arry) vertex) (cons (car arry)(cdr arry)))

       (else (cons (car arry)(addVertexToIndex (cdr arry) vertex)))))

(define (accumulate op init seq)
  (cond((null? seq)init)
       (else(op (car seq)(accumulate op init (cdr seq))))))

;;post ((indexselector ( vertexselector)))
(define (addEdgeToAdjList edge adjList IndexSelector VertexSelector) 
  (cond((null? adjList) (list (makeAdjListBlock (IndexSelector edge) (addVertexToIndex '()(VertexSelector edge)))))
       ((equal? (IndexSelector edge)(Index (firstBlockInAdjList adjList))) (cons(makeAdjListBlock (IndexSelector edge) (addVertexToIndex (vertexList(firstBlockInAdjList adjList)) (VertexSelector edge) )) (restOfAdjList adjList)))
       (else (cons(firstBlockInAdjList adjList)(addEdgeToAdjList edge (restOfAdjList adjList)IndexSelector VertexSelector)))))

(define(makeDirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y From To)) '() edgeList ))
(define(makeUndirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y To From))(makeDirectedAdjList edgeList )edgeList))

(define myEdgeList '((a b)(b a)(b 1)(b 2)(1 2)))
