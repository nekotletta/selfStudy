#lang scheme

; simple calculator that handles addition and substraction
; along with the special variable 'ans' that holds the last answer

(define (is-number x)
    (number? x)
)

(define (fold procedure accumulator list)
  (if (null? list) accumulator
      (let ((head (car list))
            (tail (cdr list)))
        (fold procedure (procedure accumulator head) tail))))

(define (list-vars vars)
    (fold (lambda (rest curr)
        (begin
            (display (string-append (symbol->string (car curr)) " = " (number->string (cadr curr)) "\n"))
            rest
        )) '() vars)
)


(define (var-defined all-vars var)
    (fold (lambda (rest curr) (if (eq? (car curr) var) #t rest)) #f all-vars)
)

(define (def var)
    (display "hihi")
)

(define (compute exp1 exp2 curr-ans op vars)
    (let (( result (op (determine-action exp1 curr-ans vars) (determine-action exp2 curr-ans vars))))
    (list result vars))
)

(define (scientific-repr num)
    (define (truncate-one-place n)
        (/ (truncate (* n 10.0)) 10.0)
    )
    (let ((e (floor (/ (log num) (log 10)))))
        (define dec (/ num (power 10 e 1.0)))
        (define d (truncate-one-place dec))
        (string-append (number->string d) "E" (number->string e))
    )
)

(define (scientific-notation b e curr-ans)
    (let ((base (determine-action b curr-ans))
          (exponent (determine-action e curr-ans)))
        (* base (power 10 exponent 1.0))
    )
)


; supports negative exponents, somewhat
(define (power pow num curr-ans)
    (letrec ((power-helper
        (lambda (base exponent)
            (cond ((= exponent 0) 1.0)
                   ((= exponent 1) base)
                   (( = base 1) 1.0)
                   ((> exponent 0) (* base (power-helper base (- exponent 1))))
                   ((< exponent 0) (/ 1 (power-helper base (* -1 exponent))))
            )
        )))
        (power-helper (determine-action pow curr-ans) (determine-action num curr-ans))
    )
)

 (define (determine-action exp curr-ans curr-vars)
    (cond ((is-number exp) (exact->inexact exp))
        ((and (symbol? exp) (eq? exp 'ans)) curr-ans)
        ((pair? exp)
            (let ((first-exp (cadr exp))
            (second-exp (caddr exp)))
            (cond 
                ((eq? (car exp) '+) (compute first-exp second-exp curr-ans +  curr-vars))
                ((eq? (car exp) '-) (compute first-exp second-exp curr-ans - curr-vars))
                ((eq? (car exp) '*) (compute first-exp second-exp curr-ans *  curr-vars))
                ((eq? (car exp) '/) (compute first-exp second-exp curr-ans / curr-vars))
                ((eq? (car exp) '^) (power first-exp second-exp curr-ans))
                ((eq? (car exp) 'E) (scientific-notation first-exp second-exp curr-ans))
                (else (error "Unknown operation"))
            ))
        ) (else (error "Unknown expression"))
    ) 
 )


(define (compile exp curr-ans vars)
    (determine-action exp curr-ans vars)
)

 (define (input-loop ans vars)
     (display "Enter expression to evaluate (or 'exit' to quit): ")
     (define input-exp (read))
     (if (eq? input-exp 'exit)
        (begin
            (display "Exiting...")
            (newline))
        (begin
             (let ((result (compile input-exp ans vars)))
                (if (or (> (car result) 100000) (< (car result) 0.00001))
                    (begin
                        (display "Result in scientific notation: ")
                        (display (scientific-repr (car result)))
                        (newline)
                    )
                    (begin
                        (display "Result: ")
                        (display (car result))
                        (newline)
                    )
                )

                (display "Your current vars are:")(newline)
                (list-vars (cadr result))(newline)
        

             (input-loop result (cadr result)))))
)

(display "Welcome to the Scheme Calculator!")(newline)
(display "Plase enter expressions in polish notation. (+ 2 4)")(newline)
(display "Supports: arithmethic operations, power (^), scientific notation (E), and ans")(newline)(newline)
(input-loop 0 '((x 12) (y 3)))