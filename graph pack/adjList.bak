(load "vertex.rkt")

(define (edgeSet-reps m)


(define (list-rep)

;;edges constructors/selectors 

;;takes 2 vertices , post (vertex , vertex)
(define (makeEdge From to)(cons From (list To)))

(define (From edge)(car edge))

(define (To edge)(cadr edge))

;;edgeList constructors/ selectors

(define(makeEmptyEdgeList) '())
(define(makeEdgeList edge) (list edge))
(define(firstInEdgeList edgeList) (car edgeList))
(define(restOfEdgeList edgeList) (cdr edgeList))
;;add edge to edgelist or make new list, repeats allowed
(define (addEdgeToEdgeList edge edgeList)  
  (cond((null? edgeList)(makeEdgeList edge))
       ((equal? (firstInEdgeList edgeList) edge) edgeList)
       (else (cons (firstInEdgeList edgeList)(addEdgeToEdgeList edge (restOfEdgeList edgeList) )))))


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


  

  (cond ((eq? m 'proc) (procedural-rep))
        ((eq? m 'list) (list-rep)))

  )


(define make-edge ((edgeSet-reps 'list) 'make-edge))
(define from ((edgeSet-reps 'list) 'from))
(define to ((edgeSet-reps 'list) 'to))
(define make-empty-edge-list ((edgeSet-reps 'list) 'make-empty-edge-list))
(define make-edge-list ((edgeSet-reps 'list) 'make-edge-list))
(define first-in-edge-list ((edgeSet-reps 'list) 'first-in-edge-list))
(define rest-of-edge-list ((edgeSet-reps 'list) 'rest-of-edge-list))
(define add-edge-to-edge-list ((edgeSet-reps 'list) 'add-edge-to-edge-list))

  (define elist (make-edge-list '(a b)))