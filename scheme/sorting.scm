#lang scheme
(define top_num 100)

(define (generate_list n)
    (if (<= n 0) '()
    (cons (random top_num) (generate_list (- n 1))))   
)

(define data_to_sort (generate_list 10))


(define merge 
    (lambda (left right)
        (cond
            ((null? left) right)
            ((null? right) left)
            ((<= (car left) (car right))
                (cons (car left)
                      (merge (cdr left) right)))
            (else
                (cons (car right)
                      (merge left (cdr right))))
        )
    )
)

(define (insert_sorted val l)
    (cond
        ((null? l) (list val))
        ((<= val (car l)) (cons val l))
        (else
            ; car l, since i need to keep building the list
            (cons (car l) (insert_sorted val (cdr l)))
    ))
)

(define (merge_sort l)
    ;quotient is to force integer division
    (let ((half (quotient (length l) 2)))
        (if (zero? half) l
            ; else (list is longer than 0)
            (merge (merge_sort (take l half))
                   (merge_sort (drop l half)))
        )
    )
)

(display "Data to sort: ")
(display data_to_sort)
(newline)
(display "Sorted data: ")
(define sorted_data (list))
(set! sorted_data (merge_sort data_to_sort))
(display sorted_data)
(newline)
(display "Value to insert: 23")
(newline)
(display (insert_sorted 23 sorted_data))
(newline)