

;; PURELY FUNCTIONAL GRAPH PACKAGE PROJECT 
;; CSC 335 SPRING 2023

;; PART 1.1

;; April 19 2023

;; The first step in building a graph package is to design an abstract
;; data type for undirected graphs.

;; If you have not already done so, you will need to read most of Section 2.1
;; in Abelson & Sussman before much of this makes sense to you.  You will
;; also benefit by reading (ahead) in the notes I have posted, through those for
;; Class 20, and from working Exercise 3 of HW7.


;; PART 1.2

;; April 23 2023

;; I am hoping that you have all started thinking about how to represent graphs.  You all
;; recall that a graph G is a pair (V, E) of a set of vertices and a set of edges.  This
;; suggests that you will need data types for sets, for vertices, and for edges.

;; It will help you design these data types if you spend some time thinking about the kinds
;; of computations one wants to carry out using them.  Here are a few suggestions, in no particular
;; order, to get you started on graphs (remember that all computations are to be purely functional):

;;; is there a path in G between vertices v1 and v2?  (Will it make sense to have a data type for
;;; paths?)

;;; find a shortest path in G between vertices v1 and v2

;;; is the graph G acyclic?

;;; is the graph G connected?

;;; find a spanning tree in G (will it be helpful to have a data type for trees?)

;;; what is the diameter of the graph G?

;;; is the graph G bipartite?

;;; what is a largest clique in the graph G?


;;; Other possibilities will be suggested later.


;;; What modifications would be necessary to implement labeled undirected graphs - that is, graphs in which
;;; each node has a label?  Can you implement depth-first and breadth-first search for such graphs?   

;;; What modifications would be necessary to implement directed graphs?  Can you implement topological sort
;;; for such graphs?  


;;; What modifications would be necessary to support computations on weighted graphs?   Would you need
;;; any additional data structures to implement an algorithm for finding a minimum spanning tree?


(define(makeEdgeList) '())

(define (makeEdge From to)(cons From (list To)))

(define (From edge)(car edge))

(define (To edge)(cadr edge))

(define (Index AdjListBlock)(car AdjListBlock))   



(define (addVertexToIndex arry vertex)

  (cond((null? arry)(list vertex))

       ((equal? (car arry) vertex) (cons (car arry)(cdr arry)))

       (else (cons (car arry)(addVertexToIndex (cdr arry) vertex)))))



                                  

(define (addEdge edge edgeList)

  (cond ((null? edgeList)(list edge))

        (else (cons edge edgeList))))

(define (firstBlockInAdjList adjList) (car adjList))
(define (restOfAdjList adjList) (cdr adjList))

(define (addEdgeToAdjList edge adjList IndexSelector VertexSelector) ;;needs directed version
  (cond((null? adjList)(list(IndexSelector edge)(list (VertexSelector edge))))
       ((equal? (IndexSelector edge)(Index (firstBlockInAdjList adjList)))(cons (addVertexToIndex (firstBlockInAdjList adjList) (VertexSelector edge))(restOfAdjList adjList)))
       (else (cons(firstBlockInAdjList adjList)(addEdgeToAdjList edge (restOfAdjList adjList)))))

       
(define (MakefirstBlockInAdjList edge isDirected) ;;needs directed version
 (if isDirected (list(list (To edge)(list (From edge)))) (addEdgeToAdjList edge (list(list (To edge)(list (From edge)) From To)))))
  
(define (firstBlockInEdgeList edgeList) (car edgeList))
(define (restOfEdgeList edgeList) (cdr edgeList))
  
(define (makeAdjList edgeList answer-so-far isDirected)

  (cond((null? edgeList) answer-so-far)

       ((null? answer-so-far) (makeAdjList (restOfEdgeList edgeList) (MakefirstBlockInAdjList (firstBlockInEdgeList edgeList)) isDirected))

       ((equal? (Index (firstBlockInAdjList answer-so-far) )  (To(firstBlockInEdgeList edgeList)))
        (if isDirected
                      (makeAdjList (restOfEdgeList edgeList) (addEdgeToAdjList(firstBlockInEdgeList edgeList) answer-so-far From To))isDirected)
                      (makeAdjList (restOfEdgeList edgeList) (addEdgeToAdjList (firstBlockInEdgeList edgeList) (addEdgeToAdjList(firstBlockInEdgeList edgeList) answer-so-far From To) To From)isDirected) )

       ((equal?(Index (firstBlockInAdjList answer-so-far)) (From(firstBlockInEdgeList edgeList)))
        (if isDirected
            (makeAdjList (restOfEdgeList edgeList) answer-so-far isDirected)
            (makeAdjList (restOfEdgeList edgeList) (addEdgeToAdjList(firstBlockInEdgeList edgeList) answer-so-far From To))isDirected))
       (else (makeAdjList (restOfEdgeList edgeList) answer-so-far isDirected))))
