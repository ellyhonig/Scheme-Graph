(load "vertex.rkt")
(load "edges.rkt")
;(load "examples.rkt")


(define (adjList-reps m)


(define (directed-rep)





;adjListblock constructor/ selectors

;takes: index vertexList , post (index vertexList)
(define (makeAdjListBlock index vertexList )
(list index vertexList) )
;takes adjlistblock, returns (adjlistblock)
(define (Index adjListBlock)(car adjListBlock))
(define (vertexList-ofAdjList adjListBlock)(cadr adjListBlock))

;;adjList constructors/ selectors
(define (makeAdjList adjListBlock)(list adjListBlock))
(define (firstBlockInAdjList adjList) (car adjList))
(define (restOfAdjList adjList) (cdr adjList))
(define (adjListBlockOfIndex adjList indexToCheck)
  (cond((null? adjList) '())
        ((equal?(Index (firstBlockInAdjList adjList)) indexToCheck) (firstBlockInAdjList adjList))
        (else (adjListBlockOfIndex (restOfAdjList adjList) indexToCheck))))
;;post ((indexselector ( vertexselector)))
(define (addEdgeToAdjList edge adjList IndexSelector VertexSelector) 
  ;is adjList empty? that means our inputted index hasnt been created. Create it.
  (cond((null? adjList) (makeAdjList (makeAdjListBlock (IndexSelector edge) (addVertexToVertexList '()(VertexSelector edge)))))
       ;is the indexselector of edge the same as index in first adjblock?
       ;make a new adjblock with the edge's vertexselector added in, cons it into restofAdjlist
       ((equal? (IndexSelector edge)(Index (firstBlockInAdjList adjList))) (cons(makeAdjListBlock (IndexSelector edge) (addVertexToVertexList (vertexList-ofAdjList(firstBlockInAdjList adjList)) (VertexSelector edge) )) (restOfAdjList adjList)))
       ;keep looking in the restOfAdjList
       (else (cons(firstBlockInAdjList adjList)(addEdgeToAdjList edge (restOfAdjList adjList) IndexSelector VertexSelector)))))


;(define (adjacentToVertex graph)
  
  

  

;;classic accumilate from the notes
(define (accumulate op init seq)
  (cond((null? seq)init)
       (else(op (car seq)(accumulate op init (cdr seq))))))
;;filter from the notes
(define (filter pred seq)
  (cond ((null? seq) seq)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))
  
       

;;just accumilate addEdgeToAdjList over edgeList to get directed graph
(define(makeDirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y From To))(make-empty-edge-list) edgeList ))


;;to get undirected, accumilate addEdgeToAdjList over edgeList but flip the index and vertex selector functions. Also have the directed version be the init value in accumilate so-
;the accumilate call is this: (accumulate addEdgeToAdjList DirectedAdjList edgeList)
(define(makeUndirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y To From))(makeDirectedAdjList edgeList )edgeList))



(define (dispatch m)
  ;;adjList specific
  (cond ((eq? m 'make-adj-list-block) makeAdjListBlock)
        ((eq? m 'index) Index)
        ((eq? m 'vertex-list-of-adj-list) vertexList-ofAdjList)
        ((eq? m 'first-block-in-adj-list) firstBlockInAdjList)
        ((eq? m 'rest-of-adj-list) restOfAdjList)
        ((eq? m 'adj-list-block-of-index) adjListBlockOfIndex)
        ((eq? m 'add-edge-to-adjList) addEdgeToAdjList)

        ;;for non adjList specific
        ((eq? m 'make-adj-list) makeDirectedAdjList)))

  dispatch)


(define (undirected-rep)





;adjListblock constructor/ selectors

;takes: index vertexList , post (index vertexList)
(define (makeAdjListBlock index vertexList )
(list index vertexList) )
;takes adjlistblock, returns (adjlistblock)
(define (Index adjListBlock)(car adjListBlock))
(define (vertexList-ofAdjList adjListBlock)(cadr adjListBlock))

;;adjList constructors/ selectors
(define (makeAdjList adjListBlock)(list adjListBlock))
(define (firstBlockInAdjList adjList) (car adjList))
(define (restOfAdjList adjList) (cdr adjList))
(define (adjListBlockOfIndex adjList indexToCheck)
  (cond((null? adjList) '())
        ((equal?(Index (firstBlockInAdjList adjList)) indexToCheck) (firstBlockInAdjList adjList))
        (else (adjListBlockOfIndex (restOfAdjList adjList) indexToCheck))))
;;post ((indexselector ( vertexselector)))
(define (addEdgeToAdjList edge adjList IndexSelector VertexSelector) 
  ;is adjList empty? that means our inputted index hasnt been created. Create it.
  (cond((null? adjList) (makeAdjList (makeAdjListBlock (IndexSelector edge) (addVertexToVertexList '()(VertexSelector edge)))))
       ;is the indexselector of edge the same as index in first adjblock?
       ;make a new adjblock with the edge's vertexselector added in, cons it into restofAdjlist
       ((equal? (IndexSelector edge)(Index (firstBlockInAdjList adjList))) (cons(makeAdjListBlock (IndexSelector edge) (addVertexToVertexList (vertexList-ofAdjList(firstBlockInAdjList adjList)) (VertexSelector edge) )) (restOfAdjList adjList)))
       ;keep looking in the restOfAdjList
       (else (cons(firstBlockInAdjList adjList)(addEdgeToAdjList edge (restOfAdjList adjList) IndexSelector VertexSelector)))))
;;undirected version
  (define (addEdgeToUndirectedAdjList edge adjList) 
   (addEdgeToAdjList edge (addEdgeToAdjList edge adjList from to) to from))

;(define (adjacentToVertex graph)
  
  

  

;;classic accumilate from the notes
(define (accumulate op init seq)
  (cond((null? seq)init)
       (else(op (car seq)(accumulate op init (cdr seq))))))
;;filter from the notes
(define (filter pred seq)
  (cond ((null? seq) seq)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))
  
       

;;just accumilate addEdgeToAdjList over edgeList to get directed graph
(define(makeDirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y from to))(make-empty-edge-list) edgeList ))
;;to get undirected, accumilate addEdgeToAdjList over edgeList but flip the index and vertex selector functions. Also have the directed version be the init value in accumilate so-
;the accumilate call is this: (accumulate addEdgeToAdjList DirectedAdjList edgeList)
(define(makeUndirectedAdjList edgeList) (accumulate (lambda(x y)(addEdgeToAdjList x y to from))(makeDirectedAdjList edgeList )edgeList))



(define (dispatch m)
  
  (cond ((eq? m 'make-adj-list-block) makeAdjListBlock)
        ((eq? m 'index) Index)
        ((eq? m 'vertex-list-of-adj-list) vertexList-ofAdjList)
        ((eq? m 'first-block-in-adj-list) firstBlockInAdjList)
        ((eq? m 'rest-of-adj-list) restOfAdjList)
        ((eq? m 'adj-list-block-of-index) adjListBlockOfIndex)
        ((eq? m 'add-edge-to-adjList) addEdgeToUndirectedAdjList)       
        ((eq? m 'make-adj-list) makeUndirectedAdjList)))

  dispatch)
  

  (cond ((eq? m 'directed) (directed-rep))
        ((eq? m 'undirected) (undirected-rep)))

  )




(define make-adj-list-block ((adjList-reps 'undirected) 'make-adj-list-block))
(define index ((adjList-reps 'undirected) 'index))
(define vertex-list-of-adj-list ((adjList-reps 'undirected) 'vertex-list-of-adj-list))
(define first-block-in-adj-list ((adjList-reps 'undirected) 'first-block-in-adj-list))
(define rest-of-adj-list ((adjList-reps 'undirected) 'rest-of-adj-list))
(define adj-list-block-of-index ((adjList-reps 'undirected) 'adj-list-block-of-index))
(define add-edge-to-adjList ((adjList-reps 'undirected) 'add-edge-to-adjList))
(define make-adj-list ((adjList-reps 'undirected) 'make-adj-list))



;(define make-adj-list ((adjList-reps 'undirected) make-adj-list))