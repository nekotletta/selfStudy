#lang scheme

; calculator built on scheme with support for:
; arithmethics, exponentiation, scientific notation,
; ans keyboard, and variable definitions and usage

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


(define (var-defined var all-vars)
    (fold (lambda (rest curr) (if (eq? (car curr) var) #t rest)) #f all-vars)
)

(define (get-var-value var all-vars)
    (if (var-defined var all-vars)
        (fold (lambda (rest curr) (if (eq? (car curr) var) (cadr curr) rest)) #f all-vars)
        #f
    )
)

(define (update-var var new-val all-vars)
  (cond
    ((null? all-vars) '()) 
    ((eq? (car (car all-vars)) var) (cons (list var new-val) (cdr all-vars)))  
    (else (cons (car all-vars) (update-var var new-val (cdr all-vars))))
)) 

(define (def new-var new-val curr-ans all-vars)
    (let (( var (determine-action new-var curr-ans all-vars))
          ( val (determine-action new-val curr-ans all-vars)))

          (if (list? val)
              (set! val (car val)) val)

        (if (var-defined var all-vars)

            (let ((updated-pair (update-var var val all-vars)))
                (list curr-ans updated-pair))

            (let ((new-pair (cons (list var val) all-vars)))
                (list curr-ans new-pair))
        )

    )
)

; the user may use variables in the computations
; if variable -> validate it exists, and get the corresponding value
; otherwise -> simply return the value the user gave
(define (resolve-var var all-vars)
    (if (and (symbol? var) (not (eq? var 'ans)))
        (if (var-defined var all-vars)
            (get-var-value var all-vars)
            (error (string-append (symbol->string var) ": Undefined variable"))
        )
    var)
)

(define (format-result val)
    (if (list? val) (car val)
    val)
)

(define (compute exp1 exp2 curr-ans op vars)

    (let ((val1 (determine-action exp1 curr-ans vars))
        (val2 (determine-action exp2 curr-ans vars)))

        ; compute returns a list of result and vars
        ; we can operate of the result of the computation
        (set! val1 (format-result val1))
        (set! val2 (format-result val2))

        (set! val1 (resolve-var val1 vars))
        (set! val2 (resolve-var val2 vars))
        ; op -> arithmethic symbol
        (let ((result (op val1 val2)))
            (list result vars))
    )
)

(define (scientific-repr num curr-vars) 
    (define (truncate-one-place n)
        (/ (truncate (* n 10.0)) 10.0)
    )
    (set! num (resolve-var num curr-vars))
    (let ((e (floor (/ (log num) (log 10)))))
        (define exp-res (power 10 e 1.0 curr-vars))
        (define dec (/ num (car exp-res)))
        (define d (truncate-one-place dec))

        (string-append (number->string d) "E" (number->string e))
    )
)

(define (scientific-notation b e curr-ans curr-vars)
    (let ((base (determine-action b curr-ans curr-vars))
          (exponent (determine-action e curr-ans curr-vars)))

          (set! base (resolve-var base curr-vars))
          (set! exponent (resolve-var exponent curr-vars))

          (let ((res (power 10 exponent 1.0 curr-vars)))
            (list (* base (car res)) curr-vars)
          )

    )
)

; supports negative exponents, somewhat
(define (power pow num curr-ans curr-vars)
    (letrec ((power-helper
        (lambda (base exponent)
            (set! base (resolve-var base curr-vars))
            (set! exponent (resolve-var exponent curr-vars))
            (cond ((= exponent 0) 1.0)
                   ((= exponent 1) base)
                   (( = base 1) 1.0)
                   ((> exponent 0) (* base (power-helper base (- exponent 1))))
                   ((< exponent 0) (/ 1 (power-helper base (* -1 exponent))))
            )
        )))
        (let ((res (power-helper (determine-action pow curr-ans curr-vars) (determine-action num curr-ans curr-vars))))
            (list res curr-vars)
        )
    )
)

(define (evaluate-logical val1 val2 comparison)
    ;(display "Evaluating logical operation")(newline)
    (cond 
        ((string=? comparison "and") 
            (cond
                ((boolean? val1) 
                    (cond 
                        ((boolean? val2) (if (and (eq? val1 #t) (eq? val2 #t)) #t #f))
                        ((number? val2) (if (and (eq? val1 #t) (> val2 0)) #t #f))
                    )
                )
            )
        )
        ((string=? comparison "or") 
            (cond
                ((boolean? val1) 
                    (cond 
                        ((boolean? val2) (if (or (eq? val1 #t) (eq? val2 #t)) #t #f))
                        ((number? val2) (if (or (eq? val1 #t) (> val2 0)) #t #f))
                    )
                )
            )
        )
        (else (error "Unknown logical operation"))
    )
)

(define (validate-condition exp1 exp2 curr-ans comparison curr-vars)
    (let ((val1 (determine-action exp1 curr-ans curr-vars))
          (val2 (determine-action exp2 curr-ans curr-vars)))

          ;(display "this my og result conditional: ")(display val1)(newline)
          (set! val1 (resolve-var val1 curr-vars))
          (set! val2 (resolve-var val2 curr-vars))

            (set! val1 (format-result val1))
            (set! val2 (format-result val2))

            ;(display "this my og result conditional: ")(display val1)(newline)
            ;(display "this my og result conditional: ")(display val2)(newline)

            (cond 
                ((eq? comparison =) (list (if (= val1 val2) #t #f) curr-vars))
                ((eq? comparison <) (list (if (< val1 val2) #t #f) curr-vars))
                ((eq? comparison >) (list (if (> val1 val2) #t #f) curr-vars))
                ((string? comparison)
                    (cond 
                        ((string=? comparison "and") (list (evaluate-logical val1 val2 "and") curr-vars))
                        ((string=? comparison "or") (list (evaluate-logical val1 val2 "or") curr-vars))
                        (else (error "Unknown logical operation"))
                    )
                )
                (else (error (string-append (symbol->string comparison) ": Unknown comparison operator")))
            )
    )
)

(define (conditional condition if-true if-false curr-ans curr-vars)


        (let ((cond-result (determine-action condition curr-ans curr-vars)))
            ;(set! cond-result (resolve-var cond-result curr-vars))
            ;(display (string-append "Condition evaluated to: " (format "~a" (car cond-result))))(newline)
        ;   (set! condition (resolve-var condition curr-vars))

           (if (eq? (car cond-result) #t)
               (begin
                (if (string? if-true) 
                    (begin (display if-true)(newline) (list curr-ans curr-vars))
                    (begin
                        ;(display "this my og result: ")(display if-true)(newline)
                        (set! if-true (determine-action if-true curr-ans curr-vars))
                        ;(display "this my mid result: ")(display if-true)(newline)
                        (set! if-true (resolve-var if-true curr-vars))

                        ;(display "this all i got: ")(display if-true)(newline)
                        (list if-true curr-vars)
                        
                        ;(list (car if-true) (cadr if-true))
                    )
                )
               )
               (begin
                (if (string? if-false) 
                    (begin (display if-false)(newline) (list curr-ans curr-vars))
                    (begin
                        ;(display "this my og result: ")(display if-false)(newline)
                        (set! if-false (determine-action if-false curr-ans curr-vars))
                        (set! if-false (resolve-var if-false curr-vars))
                        (list if-false curr-vars)
                        ;(set! if-false (format-result if-false))
                        
                    )
                )
               )
           )
        )
)

 (define (determine-action exp curr-ans curr-vars)
    (cond ((number? exp) (exact->inexact exp))
          ((boolean? exp) exp)
        ((and (symbol? exp) (eq? exp 'ans)) curr-ans)
        ((and (symbol? exp) (not (eq? exp 'ans))) exp)
        ((pair? exp)
            (let ((first-exp (cadr exp))
            (second-exp (caddr exp)))
            (cond 
                ((eq? (car exp) '+) (compute first-exp second-exp curr-ans +  curr-vars))
                ((eq? (car exp) '-) (compute first-exp second-exp curr-ans - curr-vars))
                ((eq? (car exp) '*) (compute first-exp second-exp curr-ans *  curr-vars))
                ((eq? (car exp) '/) (compute first-exp second-exp curr-ans / curr-vars))
                ((eq? (car exp) '^) (power first-exp second-exp curr-ans curr-vars))
                ((eq? (car exp) 'E) (scientific-notation first-exp second-exp curr-ans curr-vars))
                ((eq? (car exp) 'DEF) (def first-exp second-exp curr-ans curr-vars))
                ((eq? (car exp) 'IF) (conditional first-exp second-exp (cadddr exp) curr-ans curr-vars))
                ((eq? (car exp) 'EQ) (validate-condition first-exp second-exp curr-ans = curr-vars))
                ((eq? (car exp) 'LT) (validate-condition first-exp second-exp curr-ans < curr-vars))
                ((eq? (car exp) 'GT) (validate-condition first-exp second-exp curr-ans > curr-vars))
                ((eq? (car exp) 'OR) (validate-condition first-exp second-exp curr-ans "or" curr-vars))
                ((eq? (car exp) 'AND) (validate-condition first-exp second-exp curr-ans "and" curr-vars))
                (else (error (string-append (symbol->string (car exp)) ": Unknown operation")))
            ))
        ) (else (error (string-append (symbol->string (car exp)) ": Unknown expression type")))
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
                ;(display "sali del vbideojuego con: ")(display result)(newline)
                
                (if (and (not (= (car result) 0)) (or (> (abs (car result)) 100000) (< (abs (car result)) 0.00001)))
                    
                    (begin
                        (display "Result in scientific notation: ")
                        (display (scientific-repr (car result) (cdr result)))
                        (newline)
                    )

                    (begin
                        (display "Result: ")
                        (display (car result))
                        (newline)
                    ) 
                )
                (if (not (null? (cadr result)))
                (begin
                    (display "Your current vars are:")(newline)
                    (list-vars (cadr result))(newline)
                ) (newline))

             (input-loop (car result) (cadr result)))))
)

(display "Welcome to the Scheme Calculator!")(newline)
(display "Plase enter expressions in polish notation. (+ 2 4)")(newline)
(display "Supports: arithmethic operations, power (^), scientific notation (E), variables (DEF), and ans")(newline)
(display "IF syntax --> (IF (condition) (result if true) (result if false))")(newline)(newline)
(input-loop 0 '())