(define (vertexSet-reps m)


(define (list-rep)

 ;;vertex constructor
(define (make-vertex contents) contents)

;vertexList constructor/selector
;takes: vertex , post: (vertex)
(define (make-EmptyVertexList) (list))
(define (make-vertexList vertex) (list vertex))
(define (firstVertexInV-List vertexList)(car vertexList))
(define (restVertexList vertexList)(cdr vertexList))
;add vertex to vertex list, check for repeats before adding
;;post: (...vertex...)
(define (addVertexToVertexList vertexList vertex)  
  (cond((null? vertexList)(make-vertexList vertex))
       ((equal? (firstVertexInV-List vertexList) vertex) vertexList)
       (else (cons (firstVertexInV-List vertexList)(addVertexToVertexList (restVertexList vertexList) vertex)))))

 (define (dispatch m)
    (cond ((eq? m 'make-EmptyVertexList) make-EmptyVertexList)
          ((eq? m 'firstVertexInV-List) firstVertexInV-List)
          ((eq? m 'restVertexList) restVertexList)
          ((eq? m 'restVertexList) restVertexList)
          ((eq? m 'addVertexToVertexList) addVertexToVertexList)
          ))

  dispatch)


  

  (cond ((eq? m 'proc) (procedural-rep))
        ((eq? m 'list) (list-rep)))

  )

(define make-EmptyVertexList ((vertexSet-reps 'list) 'make-EmptyVertexList))
(define make-vertexList ((vertexSet-reps 'list) 'make-vertexList))
(define firstVertexInV-List ((vertexSet-reps 'list) 'firstVertexInV-List))
(define restVertexList ((vertexSet-reps 'list) 'restVertexList))
(define addVertexToVertexList ((vertexSet-reps 'list) 'addVertexToVertexList))


  