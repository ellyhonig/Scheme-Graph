(load "edges.rkt")
(load "vertex.rkt")
(load "misc-functions.rkt")
(load "stacks-queues.rkt")
(load "examples.rkt")
(load "graphs.rkt")
(define (BFS-wrapper graph starting-vertex)
  (BFS graph make-empty-stack (enqueue starting-vertex make-empty-queue))) 


(define (addVertexListToQueue vertexList queue alreadyVisitedStack)  
  (make-set (filter  (lambda (v)(not(in-set? alreadyVisitedStack v)))
          (accumulate (lambda(x y) (enqueue x y))  queue vertexList ))))


(define (BFS graph VisitedStack queue) 
  (cond ((null? queue)
         '())
        (else
         (begin
           ; Print the queue contents
           (display "queue: ")
           (display-queue queue)
           (newline)
           
           ; Print the visited vertices
           (display "visited: ")
           (display-visited (insert VisitedStack (front-of-queue queue)))
           (newline)
           
           ; Perform BFS
           (BFS graph
                (push-stack (front-of-queue queue) VisitedStack)
                (addVertexListToQueue
                 (verticies-adjacent-to-vertex (front-of-queue queue) graph)
                 (dequeue queue)VisitedStack))
           ))))

(define (display-queue queue)
  (cond ((empty-queue? queue)
         (display ""))
        (else
         (begin
           (display (front-of-queue queue))
           (display " ")
           (display-queue (rear-of-queue queue))))))

(define (display-visited stack)
  (cond ((empty-stack? stack)
         (display ""))
        (else
         (begin
           (display (top-stack stack))
           (display " ")
           (display-visited (pop-stack stack))))))
;;(BFS-wrapper myGraph '1)
;;(BFS-wrapper myGraph2 'A)
