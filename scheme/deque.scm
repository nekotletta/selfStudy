#lang scheme
; initialize front and rear stacks
(define (deque)
    (list '() '())
)

; append at the end
(define (enque-back q val)
    ;        at the front                   at the back
    (list (append (car q) (list val)) (append (list val) (cadr q)))
)
; append at the front
(define (enque-front q val)
    ;        at the front                   at the back
    (list (append (list val) (car q)) (append (cadr q) (list val)))
)

; i have two stacks, mirrors of each other
; i need to find the corresponding element 
(define (del-last q)
    (cond
        [(null? q) '()]
        [(null? (cdr q)) '()]
        [else (cons (car q) (del-last (cdr q))) ]
    )
)

; remove front
(define (attend q)
    (let ((front (car q))
        (back (cadr q)))
        
        (let ((nl (del-last back)))
        (list (cdr front) nl))
    )
)

;remove back
(define (leave q)
    (let ((front (car q))
        (back (cadr q)))
        
        (let ((nl (del-last front)))
        (list nl (cdr back)))
    )
)


(define (is-empty q)
    (and (null? (car q)) (null? (cadr q)))
)

(define (peek-front q)
    (if (is-empty q) (display "no one")
    (display (car (car q))))
)

(define (peek-rear q)
    (if (is-empty q) (display "no one")
    (display (car (cadr q))))
)

(define (waiting-time q val)
    (define (helper q val index)
        (cond
            [(null? q) #f]
            [(= (car q) val) index]
            [else (helper (cdr q) val (+ index 1))]
        )
    ) 
    (let ((wt (helper (car q) val 0)))
        (if (eq? wt #f)
            (display (string-append "the value " (number->string val) " is not present in this queue." "\n" "\n"))
            (display (string-append "it will take " (number->string wt) " turn(s) for " (number->string val) " to be attended." "\n" "\n"))
        )
    )
)

(define q (deque))
(set! q (enque-back q 5))
(set! q (enque-back q 19))
(set! q (enque-front q 12))
(display q)
(newline)
(display "next person to attend: ")
(peek-front q)
(newline)
(display "last person in line: ")
(peek-rear q)
(newline)(newline)
(waiting-time q 5)
(set! q (attend q))
(display "update queue: ")
(display q)(newline)
(display "next person to attend: ")
(peek-front q)(newline)
(display "this person will leave the line: ")
(peek-rear q)(newline)
(set! q (leave q))
(display "update queue: ")
(display q)
(set! q (leave q))
(newline)
(waiting-time q 5)