#lang scheme

; simple calculator that handles addition and substraction
; along with the special variable 'ans' that holds the last answer

(define (is-number x)
    (number? x)
)

(define (add exp1 exp2 curr-ans)
    (+ (determine-action exp1 curr-ans) (determine-action exp2 curr-ans))     
)

(define (sub exp1 exp2 curr-ans)
    (- (determine-action exp1 curr-ans) (determine-action exp2 curr-ans)) 
)

 (define (determine-action exp curr-ans)
    (cond ((is-number exp) exp)
        ((and (symbol? exp) (eq? exp 'ans)) curr-ans)
        ((pair? exp)
            (let ((first-exp (cadr exp))
            (second-exp (caddr exp)))
            (cond 
                ((eq? (car exp) '+) (add first-exp second-exp curr-ans))
                ((eq? (car exp) '-) (sub first-exp second-exp curr-ans))
                (else (error "Unknown operation"))
            ))
        ) 
    )
 )


(define (compile exp curr-ans)
    (determine-action exp curr-ans)
)

 (define (input-loop ans)
     (display "Enter expression to evaluate (or 'exit' to quit): ")
     (define input-exp (read))
     (if (eq? input-exp 'exit)
        (begin
            (display "Exiting...")
            (newline))
        (begin
             (let ((result (compile input-exp ans)))
                (display "Result: ")
                (display result)(newline)

             (input-loop result))))
)

(input-loop 0)