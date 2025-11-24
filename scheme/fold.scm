#lang scheme

(define (fold procedure accumulator list)
  (if (null? list) accumulator
      (let ((head (car list))
            (tail (cdr list)))
        (fold procedure (procedure accumulator head) tail))))

   
(define traverse (fold (lambda (a b) (cons b a)) (list) (list 1 2 3 4 5)))

(define less-than (fold (lambda (a b) (if (< b 3) (cons b a) a)) (list) (list 1 2 3 4 5)))

(define mult (fold (lambda (a b) (cons (* 2 b) a)) (list) (list 1 2 3 4 5)))

(define sum (fold (lambda (a b) (cons (+ b (car a)) a)) (list 0) (list 1 2 3 4 5)))

(define min (fold (lambda (a b) (if (< b a) b a)) 0 (list 1 2 -3 4 5)))

(define square (fold (lambda (a b) (if (< b 0) (cons (/ 1 (* b b)) a) (cons (* b b) a))) 
                  (list) (list 1 2 -3 4 5)))

(define n 5)
(define custom-power 
    (fold (lambda (a b)
          (letrec ((power
                (lambda (b times)
                    (if (= times 0) 1
                    (* b (power b (- times 1)))
                )
            )))
            (let ((res (power b n)))
              (cons res a))))
        (list)
        (list 1 2 -3 4 5)
    )
)

(define fact
    (fold (lambda (a b)
        (letrec ((const-fact
            (lambda (limit)
                (if (= limit 1) 1
                (* limit (const-fact (- limit 1)))))))
        (let ((res (const-fact b)))
        (cons res a))))
    (list)
    (list 1 2 3 4 5)))

 (define n 5)
(define custom-power 
    (fold (lambda (a b)
          (letrec ((power
                (lambda (b times)
                    (if (= times 0) 1
                    (let t ((multb b b)
                    (t (power b (- times 1))))
                )
            )))
            (let ((res (power b n)))
              (cons res a))))
        (list)
        (list 1 2 -3 4 5)
    )
))
(display custom-power)