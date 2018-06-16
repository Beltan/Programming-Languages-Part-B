#lang racket
(provide (all-defined-out))

;1
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

;2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: n must be non-negative")]
        [(null? xs) (error "list-nth-mod: list must be non-empty")]
        [#t (let* ([len (length xs)]
                   [index (remainder n len)])
              (car (list-tail xs index)))]))

;4
(define (stream-for-n-steps s n)
  (if (= n 0)
      `()
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;5
(define funny-number-stream
  (letrec ([f (lambda (n)
                (cond [(< n 0) (cons n (lambda () (f (+ (* n -1) 1))))]
                      [(= (remainder (+ n 1) 5) 0) (cons n (lambda () (f (* (+ n 1) -1))))]
                      [else (cons n (lambda () (f (+ n 1))))]))])
    (lambda () (f 1))))

;6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" (lambda () (dog))))]
           [dog (lambda () (cons "dog.jpg" (lambda () (dan))))])
    (lambda () (dan))))

;7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons 
                           (cons 0 (car (s))) 
                           (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons
                           (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (if (= n (vector-length vec))
                              #f
                              (let ([element (vector-ref vec n)])
                                (cond [(not (pair? element)) (f (+ n 1))]
                                      [(equal? (car element) v) element]
                                      [else (f (+ n 1))]))))])
    (f 0)))

;10
(define (cached-assoc xs n)
  (letrec ([index 0]
           [cache (make-vector n #f)]
           [f (lambda(v)
                (let ([element (vector-assoc v cache)])
                  (if element
                      element
                      (let ([value (assoc v xs)])
                        (vector-set! cache index value)
                        (set! index (remainder (+ index 1) n))
                        value))))])
    f))

;11 Challenge
(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
     (let ([z x])
       (letrec ([helper (lambda ()
                          (let ([w y])
                            (if (or (not (number? w)) (>= w z))
                                #t
                                (helper))))])
         (helper))))))
