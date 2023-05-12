


;;edges constructors/selectors 
(define(makeEdgeList) '())

(define (makeEdge From to)(cons From (list To)))

(define (From edge)(car edge))

(define (To edge)(cadr edge))

  


;add vertex to vertex list within adjList block, check for repeats before adding
;;post: (...vertex...)
(define (addVertexToIndex arry vertex)
  
  (cond((null? arry)(list vertex))

       ((equal? (car arry) vertex) (cons (car arry)(cdr arry)))

       (else (cons (car arry)(addVertexToIndex (cdr arry) vertex)))))



                                  
;;add edge to edgelist, repeats allowed
(define (addEdge edge edgeList)

  (cond ((null? edgeList)(list edge))

        (else (cons edge edgeList))))


;;selector for index of adjListBlock
(define (Index adjListBlock)(car adjListBlock))
(define (vertexList adjListBlock)(cadr adjListBlock))
;;selectors for adjLists
(define (firstBlockInAdjList adjList) (car adjList))
(define (restOfAdjList adjList) (cdr adjList))

;;selectors for EdgeLists
(define (firstBlockInEdgeList edgeList) (car edgeList))
(define (restOfEdgeList edgeList) (cdr edgeList))

;;post ((indexselector ( vertexselector)))
(define (addEdgeToAdjList edge adjList IndexSelector VertexSelector) 
  (cond((null? adjList)(list(list(IndexSelector edge)(list (VertexSelector edge)))))
       ((equal? (IndexSelector edge)(Index (firstBlockInAdjList adjList))) (cons(cons (IndexSelector edge) (addVertexToIndex (vertexList(firstBlockInAdjList adjList)) (VertexSelector edge) )) (restOfAdjList adjList)))
       (else (cons(firstBlockInAdjList adjList)(addEdgeToAdjList edge (restOfAdjList adjList)IndexSelector VertexSelector)))))

;;makes initial adj list with only edge as its contents. ie ((from (To))    
(define (MakefirstBlockInAdjList edge isDirected) 
 (if isDirected (list(list (From edge)(list (To edge)))) (addEdgeToAdjList edge (list(list (To edge)(list (From edge)))) To From)))
  




 
  
(define (makeAdjList edgeList answer-so-far isDirected)

  (cond((null? edgeList) answer-so-far)

       ((null? answer-so-far) (makeAdjList (restOfEdgeList edgeList) (MakefirstBlockInAdjList (firstBlockInEdgeList edgeList)isDirected) isDirected))

       ((equal? (Index (firstBlockInAdjList answer-so-far) )  (To(firstBlockInEdgeList edgeList)))
        (if isDirected
                      (makeAdjList (restOfEdgeList edgeList) (addEdgeToAdjList(firstBlockInEdgeList edgeList) answer-so-far From To)isDirected)
                      (makeAdjList (restOfEdgeList edgeList) (addEdgeToAdjList (firstBlockInEdgeList edgeList) (addEdgeToAdjList(firstBlockInEdgeList edgeList) answer-so-far From To) To From)isDirected) )
)
       ((equal?(Index (firstBlockInAdjList answer-so-far)) (From(firstBlockInEdgeList edgeList)))
        (if isDirected
            (makeAdjList (restOfEdgeList edgeList) answer-so-far isDirected)
            (makeAdjList (restOfEdgeList edgeList) (addEdgeToAdjList(firstBlockInEdgeList edgeList) answer-so-far From To)isDirected ) ))
       (else (makeAdjList (restOfEdgeList edgeList) answer-so-far isDirected))))


(define myEdgeList '((a b)(b a))
                    )
  
;(makeAdjList myEdgeList '() #f) 

(define myEdgeList2 '((a b)(e h)))
                    


  
