(load "vertex.rkt")
(load "edges.rkt")
(load "adjList.rkt")
(load "sets.rkt")

(define (graph-reps m)


(define (adjList-rep)

;;graph constructor/selector
(define (MakeEmptyGraph) (list (make-EmptyVertexList) (make-empty-edge-list)))
;(define (MakeEmptyGraph) (instert (make-empty-set) (make-EmptyVertexList)) ;;;;;usage of sets
(define (Makegraph vertexList edgeList) (list vertexList edgeList))
(define (vertexListOfGraph graph)(car graph))
(define (edgeListOfGraph graph)(cadr graph))

;;returns list of vertices that are adjacent to inputted vertex
  (define (VerticiesAdjToVertex vertex graph)
    (vertex-list-of-adj-list(adj-list-block-of-index(make-adj-list(edgeListOfGraph graph))vertex)))
    
 


(define (dispatch m)
  (cond ((eq? m 'make-empty-graph) MakeEmptyGraph)
        ((eq? m 'make-graph) Makegraph)
        ((eq? m 'vertex-list-of-graph) vertexListOfGraph)
        ((eq? m 'edge-list-of-graph) edgeListOfGraph)
        ((eq? m 'verticies-adjacent-to-vertex) VerticiesAdjToVertex)))


  dispatch)

(define (matrix-rep)

;;graph constructor/selector
(define (MakeEmptyGraph) (list (make-EmptyVertexList) (make-empty-edge-list)))
(define (Makegraph vertexList edgeList) (list vertexList edgeList))
(define (vertexListOfGraph graph)(car graph))
(define (edgeListOfGraph graph)(cadr graph))


(define (dispatch m)
  (cond ((eq? m 'make-edge) makeEdge)
        ((eq? m 'from) From)
        ((eq? m 'to) To)
        ((eq? m 'make-empty-edge-list) makeEmptyEdgeList)
        ((eq? m 'make-edge-list) makeEdgeList)
        ((eq? m 'first-in-edge-list) firstInEdgeList)
        ((eq? m 'rest-of-edge-list) restOfEdgeList)
        ((eq? m 'add-edge-to-edge-list) addEdgeToEdgeList)))


  dispatch)
  

  (cond ((eq? m 'list) (adjList-rep))
        ((eq? m 'matrix) (matrix-rep)))

  )


(define make-empty-graph ((graph-reps 'list) 'make-empty-graph))
(define make-graph ((graph-reps 'list) 'make-graph))
(define vertex-list-of-graph ((graph-reps 'list) 'vertex-list-of-graph))
(define edge-list-of-graph ((graph-reps 'list) 'edge-list-of-graph))
(define verticies-adjacent-to-vertex ((graph-reps 'list) 'verticies-adjacent-to-vertex))


;; (verticies-adjacent-to-vertex 'a '((a b)((a b)(b a))))