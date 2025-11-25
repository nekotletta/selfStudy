#lang scheme


(define (is-number x)
    (number? x)
)

(define (add exp1 exp2)
    (+ (determine-action exp1) (determine-action exp2))     
)

(define (sub exp1 exp2)
    (- (determine-action exp1) (determine-action exp2)) 
)

(define (determine-action exp)
     (cond ((is-number exp) exp)

           ((eq? (car exp) '+) (add (cadr exp) (caddr exp)))
           ((eq? (car exp) '-) (sub (cadr exp) (caddr exp)))
           (else (error "Unknown operation")))
)


(define (compile exp)
    (determine-action exp)
)

; used lambda as a parameter to assign stuff to it
; inside the function, huh
; (define (up x)
;     (x 5)
; )

; (display (up (lambda (val) val)))
; (newline)


; (define a '(("-" ("+" 3 4) 6) ("-" 10 5)))
; (display (car a))
; (newline)
; (display (cadr a))
(display (compile '(+ (+ 2 4) 4)))
(newline)
(display (compile '(- (+ 2 12) 4)))
(newline)


 (display "Enter expression to evaluate: ")
 (define input-exp (read))
 (display (compile input-exp))
 (newline)


; (newline)
; (display (compile '("-" 10 5)))
; (newline)