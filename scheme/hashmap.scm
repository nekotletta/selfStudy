#lang scheme

(define (hashmap)
  '()
)

(define (check search_hash key index)
    (cond
        ((null? search_hash) -1)
        ((= (car (car search_hash)) key) index)
        (else (check (cdr search_hash) key (+ 1 index)))
    )
)

(define (get_value hash key)
    (cond
        ((null? hash) #f)
        ((= (car (car hash)) key) (cadr (car hash)))
        (else (get_value (cdr hash) key))
    )
)

(define (key_in_map hash key)
    (let ((result (check hash key 0)))
        (if (= result -1) (begin (display "key not found")(newline))
        (begin (display "key found")(newline))
        )
    )
)

(define (add hash key value)
    
    (define (update_key updated_map result new_pair)
        (cond
            ((null? updated_map) '()) 
            ((= result 0) (cons new_pair (cdr updated_map))) 
            (else
                (cons (car updated_map) (update_key (cdr updated_map) (- result 1) new_pair))
            )
        )
    )
    (let ((updated_map hash)
        (result (check hash key 0))
        (new_pair  (list key value))
        )
        (if (= result -1) (set! updated_map (cons new_pair updated_map))
        (set! updated_map (update_key updated_map result new_pair))
        )
        ; (display updated_map)(newline)
        updated_map
    ) 
)

(define (delete hash key)
    (define (remove_key updated_map result)
        (cond
            ((null? updated_map) '())  
            ((= result 0) (cdr updated_map)) 
            (else (cons (car updated_map) (remove_key (cdr updated_map) (- result 1))))
        )
    )
    (let ((updated_map hash)
        (result (check hash key 0))
        )
        (if (= result -1)
            (begin
                (display "key not found, cannot delete")(newline)
                updated_map
            )
        (set! updated_map (remove_key updated_map result))
        )
        ; (display updated_map)(newline)
        updated_map
    ) 
)

(define hs (hashmap))
(define hs1 (add hs '1 1))
(display "added first key")(newline)
(display hs1)(newline)
(define hs2 (add hs1 '4 2))

(display hs2) 
(newline)
(define hs3 (add hs2 '1 3)) 
(display "after updating key '1'")(newline)
(display hs3)(newline)

(define val1 (get_value hs3 '4))
(display "value for key '4': ")(display val1)(newline)

(key_in_map hs3 '1) 
(key_in_map hs3 '5) 

(define hs4 (delete hs3 '4)) ; delete key 'b
(display "after deleting key '4'")(newline)
(display hs4)(newline)