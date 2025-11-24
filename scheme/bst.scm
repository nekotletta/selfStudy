#lang scheme
; https://www.cs.utoronto.ca/~ajuma/326f08/13Scheme6.pdf
(define (bst_const val)
    (list val '() '())
)

(define (insert val old_tree)
    (cond
        ((null? old_tree) (bst_const val))
        ( else
            ; list traversal: https://stackoverflow.com/questions/23177918/scheme-loop-through-a-list
            (let ((root (car old_tree))
                (left (cadr old_tree))
                (right (caddr old_tree)))
                ; body inside a let: required otherwise it breaks
                (cond
                    ; value is less than the root
                    ((< val root)  (list root (insert val left) right ))
                    ((>= val root) (list root left (insert val right)))

                )
            )
        )
    )
)

(define-syntax if*
   (syntax-rules (else)
      ((if* condition consequent else alternative)
       (if condition consequent alternative))))


(define (inorder tree)
    (if* (null? tree) '()
     else
        (begin
           (display (car tree))
            (newline)
            (inorder (cadr tree))
            (inorder (caddr tree))
        )
    )
)

(define (preorder tree)
    (if* (null? tree) '()
     else
        (begin
            (preorder (cadr tree))
            (display (car tree))
            (newline)
            (preorder (caddr tree))
        )
    )
)

(define (create-sublist-index src index l2)
    (if* (null? src) (list l2)
    else(
        (cond
            ((= (car src) index) (list src l2))
            ((car src) (create-sublist-index (cdr src) index l2))
        )
    )
    )
)

; (display (create-sublist-index '(1 2 3) 2 '(3 4))) 


; (define (level-order src_l res_l . index)
;     ; the list is empty (no tree to traverse)
;     (if (null? src_l) res_l
;         ; else
;         (cond
;             [(<= (length res_l index)) (update-level res_l index)]
;             (let ((upd_res (update-level res_l index (car old_tree))))
;                 ((root (car old_tree))
;                 (left (cadr old_tree))
;                 (right (caddr old_tree)))
;                 ; body inside a let: required otherwise it breaks
;                 (level-order left upd_res (+ index 1))
;                 (level-order right upd_res (+ index 1))
;             )
;         )
;     )
; )


(define (update-level l index . val)
  (let ((len (length l)))
    (if (>= index len)
        ;; index beyond list length: add an empty list at the end
        (append l (list '()))
        ;; else, proceed with normal update
        (cond
          [(null? l) '()]  ; should not happen if index < length
          [(= index 0)
           (cons (append (car l) (list val))
                 (update-level (cdr l) (- index 1) val))]
          [else
           (cons (car l)
                 (update-level (cdr l) (- index 1) val))]))))

(define (is-balanced tree)
  (if (null? tree)
      (cons #t 0) ; empty tree is balanced, height 0
      (let ((left-result (is-balanced (cadr tree)))
            (right-result (is-balanced (caddr tree))))
        (let ((left-balanced (car left-result))
              (left-height (cdr left-result))
              (right-balanced (car right-result))
              (right-height (cdr right-result)))
          (if (or (not left-balanced)
                  (not right-balanced)
                  (> (abs (- left-height right-height)) 1)) (cons #f 0) ; not balanced
              (cons #t (+ 1 (max left-height right-height))))))))

;idk why this doesnt work lol 
;(define (list2tree l . tree)
;    (cond
;        [(null? l) tree]
;        [else (list2tree (cdr l) (insert (car l) tree))]
;    )
;)
; define empty tree first
(define b_tree '())
; test list to insert
(define nodes '(1 2 4 6 24 2 5 1 5 7 9 45 23 86 23 74 35))
;(define b_tree (list2tree nodes))
(set! b_tree (bst_const 10))
;set to keep updating the tree to keep inserting
(set! b_tree (insert 5 b_tree))
(set! b_tree (insert 1 b_tree))
(set! b_tree (insert 3 b_tree))
(set! b_tree (insert 60 b_tree))
; (set! b_tree (insert 7 b_tree))
(display b_tree)
(newline)

(display "preorder traversal: \n")
(display (preorder b_tree))
(newline)
(display "inorder traversal: \n")
(display (inorder b_tree))
(newline)
; (display (level-order b_tree '() 0))
(is-balanced b_tree)