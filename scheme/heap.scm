
#lang scheme
(define (minheap)
    '()
)

; help find k smallest and
; value at index for heapify
(define (findHelper heap index)
    (if (null? heap) '()
        (if (= 0 index) (car heap)
            (findHelper (cdr heap) (- index 1))
        )
    )
)

(define (value-found heap val . index)
    (let ((index-set (if (null? index) #f (car index))))
        (cond
            ((null? heap) #f)
            ((= (car heap) val)
                (if (eq? #f index-set) #t
                    index-set
                )
            )
            (else 
                (if (eq? #f index-set) (value-found (cdr heap) val )
                    (value-found (cdr heap) val (+ index-set 1))
                )
            )
        )
    )
)
; no need to add the val as a param,
; it's already in the outer function, plus i dont change it,
; as opposed to the list, which im iterating through
(define (find heap val)
    (if (value-found heap val) (display (string-append "Value " (number->string val) " is in the heap.\n"))
        (display (string-append "Value " (number->string val) " is not present the heap.\n"))
    )
)

(define (remove heap val)
  (let ((result (value-found heap val 0)))
    (if (not (eq? result #f))
        (begin
            (display "Found at index: ") (display result) (newline)
            ; put it at the end of the list
            (set! heap (swap-index result (- (length heap) 1) heap))
            (set! heap (drop-right heap 1))
            (set! heap (heapSort heap))
            (set! result heap)
        )
        (begin
            (display "Value not present in heap. Unable to remove") (newline)
            ; set result back to original heap to not cause issues
            (set! result heap)
        )
    ) result)
)

(define (swap-index idx1 idx2 l)
  (define (helper current-idx lst)
    (cond
      ((null? lst) '())  ; end of list
      ((= current-idx idx1)
       ;; at idx1: insert element from idx2
       (cons (list-ref l idx2)
             (helper (+ current-idx 1) (cdr lst))))
      ((= current-idx idx2)
       ;; at idx2: insert element from idx1
       (cons (list-ref l idx1)
             (helper (+ current-idx 1) (cdr lst))))
      (else
       ;; other positions: keep original
       (cons (car lst)
             (helper (+ current-idx 1) (cdr lst))))))
  (helper 0 l))

(define (heapify heap limit index)
    (let ( (max-val (list-ref heap index )) 
          (left-index  (+ (* 2 index) 1) )
          (right-index (+ (* 2 index) 2) )
          (left-val (if (and (< (+ (* 2 index) 1) limit)) (list-ref heap (+ (* 2 index) 1)) #f))
          (right-val (if (and (< (+ (* 2 index) 2) limit)) (list-ref heap (+ (* 2 index) 2)) #f))
          (index-val (list-ref heap index))
          (max-index index)
        )

        (when (and (< left-index limit) (not (eq? left-val #f)) (< max-val left-val))
                (set! max-index left-index)
                (set! max-val (list-ref heap max-index))
        )
        (when (and (< right-index limit) (not (eq? right-val #f)) (< max-val right-val))
                (set! max-index right-index)
                (set! max-val (list-ref heap max-index))
        )

        (when (not (= max-index index))
            ; swapping, according to python
            (set! heap (swap-index index max-index heap))
            (set! heap (heapify heap limit max-index))
        ) heap ; return the modified list
    ) 
    
)


(define (heapify-helper heap n)
    (define (helper idx heap)
        (if (= idx -1) heap
       ;all of this is the else
        (helper (- idx 1) (heapify heap n idx))
        )
    )
    (helper (- n 1) heap) 
)

(define (sort-helper nl n)
    (define (helper idx nl)
        (if (= idx 0) nl
            (let ((nl2 (swap-index (- idx 1) 0 nl)))
                (helper (- idx 1) (heapify nl2 (- idx 1) 0))
            )
        )
    )
    (helper n nl)
)
  
(define (heapSort heap)
    ; ensure the maxheap structure is maintained
    ; (parent is more than children)
     (let ((n (length heap)))
    ;; Build max heap
    (define nl (heapify-helper heap n))
    ;; Heap sort
    (define sorted (sort-helper nl n))
    sorted)
)


(define (insert heap val)
    (cons val heap)
)

(define (find-k-largest heap k)
    (define (k-helper sorted-heap)
        (cond
            ((= (length sorted-heap) k) 
                (display "The k largest element is ")(display (car sorted-heap))(newline)
            )
            (else (find-k-largest (cdr sorted-heap) k))
        )
    )
    ; i only need to sort it once, not in every call
    (set! heap (heapSort heap))
    (k-helper heap)
    heap
)

(define heap (minheap))
(set! heap (insert heap 3))
(set! heap (insert heap 2))
(set! heap (insert heap 3))
(set! heap (insert heap 1))
(set! heap (insert heap 2))
(set! heap (insert heap 4))
(set! heap (insert heap 5))
(set! heap (insert heap 5))
(set! heap (insert heap 6))
(set! heap (reverse heap))
(display "pre sort list: ")(display heap)(newline)

(set! heap (find-k-largest heap 4))
(display heap)(newline)

