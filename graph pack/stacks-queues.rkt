(define (stack-reps m)


(define (list-rep)


;;stack stuff
(define make-empty
    '())

  (define (push x s)
    (cons x s))

  (define (top s)
    (car s))

  (define (pop s)
    (cdr s))

  (define (empty? s)
    (null? s))
(define (inStack? stack element)
  (cond((empty? stack)#f)
       ((equal? (top stack) element) #t)
       (else( inStack? (pop stack) element))))


  
(define (dispatch m)
    (cond ((eq? m 'make-empty) make-empty)
          ((eq? m 'push) push)
          ((eq? m 'top) top)
          ((eq? m 'pop) pop)
          ((eq? m 'in-stack?) inStack?)
          ((eq? m 'empty?) empty?)))

  dispatch)


  

  (cond ((eq? m 'proc) (procedural-rep))
        ((eq? m 'list) (list-rep)))

  )

(define (queue-reps m)


(define (list-rep)

;;;;;;;;queue stuff
(define makeEmptyQueue '())
(define (enqueue element queue)(cond((null? element)(queue))(else (append queue (list element)))))
(define (dequeue queue) (cdr queue))
(define (frontOfQueue queue)
  (cond ((emptyQueue? queue)
         '())
        (else(car queue))))
(define (rear-of-queue queue)(cond ((emptyQueue? queue)
         '())
        (else(cdr queue))))
(define (emptyQueue? queue) (null? queue))


(define (dispatch m)
  (cond ((eq? m 'make-empty-queue) makeEmptyQueue)
        ((eq? m 'enqueue) enqueue)
        ((eq? m 'dequeue) dequeue)
        ((eq? m 'empty-queue?) emptyQueue?)
        ((eq? m 'rear-of-queue) rear-of-queue)
        ((eq? m 'front-of-queue) frontOfQueue)))
  dispatch)


  

  (cond ((eq? m 'proc) (procedural-rep))
        ((eq? m 'list) (list-rep)))

  )
;;stack
(define make-empty-stack ((stack-reps 'list) 'make-empty))
(define push-stack ((stack-reps 'list) 'push))
(define top-stack ((stack-reps 'list) 'top))
(define pop-stack ((stack-reps 'list) 'pop))
(define in-stack? ((stack-reps 'list) 'in-stack?))
(define empty-stack? ((stack-reps 'list) 'empty?))
;;queue
(define make-empty-queue ((queue-reps 'list) 'make-empty-queue))
(define enqueue ((queue-reps 'list)'enqueue))
(define dequeue ((queue-reps 'list) 'dequeue))
(define front-of-queue ((queue-reps 'list) 'front-of-queue))
(define rear-of-queue ((queue-reps 'list) 'rear-of-queue))
(define empty-queue? ((queue-reps 'list) 'empty-queue?))