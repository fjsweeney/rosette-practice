#lang rosette

;; practice constraint programming: Sudoku Solver

;; For sudoku, we have constraints on the contents of rows, boxes, and columns
;; so these are functions that get those groups:

(define (rows p)
  p)

(define (columns p)
  (apply (curry map list) p))

(define (get-cell a b p)
  (let ([x (+ (remainder b 3) (* 3 (remainder a 3)))]
        [y (+ (quotient  b 3) (* 3 (quotient  a 3)))])
    (list-ref (list-ref p y) x)))

(define (boxes p)
  (build-list 9 (lambda (a)
                  (build-list 9 (lambda (b)
                                  (get-cell a b p))))))


;; For constraint solving, we create a symbolic data-type that represents valid values for our sudoku:
(define (sym-int)
  (define-symbolic* i integer?)
  ;; each sudoku integer is between 1 and 9 inclusive, so we assert that:
  (assert (> i 0))
  (assert (< i 10))
  i)

;; Using our symbolic data type, we can define our puzzle:
;; A sudoku puzzle is 9 by 9, and our data-type fills all cells:
(define puzzle
  (build-list 9 (thunk*
                 (build-list 9 (thunk*
                                (sym-int))))))

;; Our rules for sudoku: For each row, column, and box, there can be no duplicates of any number.
;; So we create a function that asserts there's no duplicates:
(define (check-unique list)
  (assert (eq? 9 (length (remove-duplicates list)))))

;; And we apply that function to each part of the sudoku puzzle:
(map check-unique (rows puzzle))
(map check-unique (columns puzzle))
(map check-unique (boxes puzzle))

;; That's it!


;; So, let's test the solver on a puzzle:
(define unsolved
 '((_ _ _ 8 _ 1 _ _ _)
   (_ _ _ _ _ _ 4 3 _)
   (5 _ _ _ _ _ _ _ _)
   (_ _ _ _ 7 _ 8 _ _)
   (_ _ _ _ _ _ 1 _ _)
   (_ 2 _ _ 3 _ _ _ _)
   (6 _ _ _ _ _ _ 7 5)
   (_ _ 3 4 _ _ _ _ _)
   (_ _ _ 2 _ _ 6 _ _)))

;; For loading the puzzle into our empty template, 
;; we will just assert that the concrete numerical values need to be in those positions:
(define (load-puzzle p)
  (build-list 9 (lambda (y)
                  (build-list 9 (lambda (x)
                                  (cond ([integer? (list-ref (list-ref p y) x)]
                                         (assert (eq? (list-ref (list-ref p y) x)
                                                      (list-ref (list-ref puzzle y) x)))
                                         (list-ref (list-ref p y) x))
                                        ;; If there is no number, then don't do anything.
                                        (#t '_)))))))

(load-puzzle unsolved)

;; finally, we ask the computer to find a solution based on those constraints:
(evaluate puzzle (solve #t))
