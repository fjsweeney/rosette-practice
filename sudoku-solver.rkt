#lang rosette

;; practice constraint programming

(define (int)
  (define-symbolic* i integer?)
  ;; each sudoku integer is between 1 and 9 inclusive, so we assert that:
  (assert (> i 0))
  (assert (< i 10))
  i)

;; A sudoku puzzle is 9 by 9:
(define puzzle
  (build-list 9 (thunk*
                 (build-list 9 (thunk*
                                (int))))))

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

;; let's test the solver on a puzzle:
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
;; we will just assert that the numerical values need to be in those positions:
(define (load-puzzle p)
  (build-list 9 (lambda (y)
                  (build-list 9 (lambda (x)
                                  (cond ([integer? (list-ref (list-ref p y) x)]
                                         (assert (eq? (list-ref (list-ref p y) x)
                                                      (list-ref (list-ref puzzle y) x)))
                                         (list-ref (list-ref p y) x))
                                        (#t '_)))))))

(load-puzzle unsolved)

;; Back to our constraints for rows, columns and boxes: there can be no duplicates:
(define (check-unique list)
  (assert (eq? 9 (length (remove-duplicates list)))))
  
(map check-unique (rows puzzle))
(map check-unique (columns puzzle))
(map check-unique (boxes puzzle))

;; finally, we ask the computer to find a solution based on those constraints:
(evaluate puzzle (solve #t))
