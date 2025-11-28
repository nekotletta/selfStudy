; I DIDNT WRITE THIS WHATSOEVER
; COPY PASTED FOR READING AND LEARNING PURPOSES
(import (scheme base)
        (scheme read)
        (scheme write))

(define (evaluate continuation environment expression)
  (cond ((number? expression)
         (continuation environment expression))
        ((symbol? expression)
         (let ((binding (assoc expression environment)))
           (unless binding
             (error "Unbound Variable" expression environment))
           (continuation environment (cdr binding))))
        ((equal? '- (list-ref expression 0))
         (let ((argument-0 (list-ref expression 1))
               (argument-1 (list-ref expression 2)))
           (evaluate (lambda (environment value-0)
                       (evaluate (lambda (environment value-1)
                                   (continuation environment (- value-0 value-1)))
                                 environment
                                 argument-1))
                     environment
                     argument-0)))
        ((equal? 'define (list-ref expression 0))
         (let ((variable (list-ref expression 1))
               (expression (list-ref expression 2)))
           (evaluate (lambda (environment value)
                       (let ((binding (cons variable value)))
                         (continuation (cons binding environment) value)))
                     environment
                     expression)))))

(display (evaluate (lambda (environment value) value) '() (read)))
(newline)







(import (scheme base)
        (scheme read)
        (scheme write))

(define (evaluate environment expression)
  (cond ((number? expression)
         expression)
        ((symbol? expression)
         (let ((binding (assoc expression environment)))
           (unless binding
             (error "Unbound Variable" expression environment))
           (cdr binding)))
        ((equal? '- (list-ref expression 0))
         (let ((argument-0 (evaluate environment (list-ref expression 1)))
               (argument-1 (evaluate environment (list-ref expression 2))))
           (- argument-0 argument-1)))
        ((equal? 'let (list-ref expression 0))
         (let ((variable   (list-ref (list-ref expression 1) 0))
               (initialize (list-ref (list-ref expression 1) 1))
               (expression (list-ref expression 2)))
           (let ((value (evaluate environment initialize)))
             (evaluate (cons (cons variable value) environment) expression))))))

(display (evaluate '() (read)))
(newline)








(import (scheme base)
        (scheme read)
        (scheme write))

(define (evaluate continuation environment expression)
  (cond ((number? expression)
         (continuation environment expression))
        ((symbol? expression)
         (let ((binding (assoc expression environment)))
           (unless binding
             (error "Unbound Variable" expression environment))
           (continuation environment (cdr binding))))
        ((equal? '- (list-ref expression 0))
         (let ((argument-0 (list-ref expression 1))
               (argument-1 (list-ref expression 2)))
           (evaluate (lambda (environment value-0)
                       (evaluate (lambda (environment value-1)
                                   (continuation environment (- value-0 value-1)))
                                 environment
                                 argument-1))
                     environment
                     argument-0)))
        ((equal? 'define (list-ref expression 0))
         (let ((variable (list-ref expression 1))
               (expression (list-ref expression 2)))
           (evaluate (lambda (environment value)
                       (let ((binding (cons variable value)))
                         (continuation (cons binding environment) value)))
                     environment
                     expression)))
        ((equal? 'let (list-ref expression 0))
         (let ((variable   (list-ref (list-ref expression 1) 0))
               (initialize (list-ref (list-ref expression 1) 1))
               (expression (list-ref expression 2)))
           (evaluate (lambda (environment^ value)
                       (let ((binding (cons variable value)))
                         (evaluate (lambda (environment^ value)
                                     (continuation environment value))
                                   (cons binding environment^)
                                   expression)))
                     environment
                     initialize)))
        ((equal? 'exit (list-ref expression 0))
         (let ((argument (list-ref expression 1)))
           (evaluate (lambda (environment value)
                       value)
                     environment
                     argument)))))

(display (evaluate (lambda (environment value) value) '() (read)))
(newline)