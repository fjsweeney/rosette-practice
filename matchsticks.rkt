#lang rosette

(define case1 '(12 12 12 12))
(define case2 '(5 5 5 5 4 4 4 4 3 3 3 3))

(define (sum l)
  (apply + l))

(define (check input)
  (begin
    (define padded (append input (build-list (* 3 (length input)) (thunk* 0))))
    (define sides (build-list 4 (thunk*
                                 (build-list (length input)
                                             (thunk* (begin
                                                       (define-symbolic* i integer?)
                                                       i))))))
    (define vars (apply append sides))
    (define solver (solve+))
    
    (solver (andmap (lambda (n) (eq? (/ (apply + input) 4) n)) (map sum sides)))
    (solver (andmap eq? (sort vars >) (sort padded >)))
    (define solution2 (solver #t))
    (cond
      ([unsat? solution2] #f)
      (#t (evaluate sides solution2)))
    ))

(check case1)
(check case2)
