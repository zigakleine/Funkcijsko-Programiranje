#lang racket

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (fib x)
  (cond [(= x 1) 1]
        [(= x 2) 1]
        [#t (+ (fib (- x 1)) (fib (- x 2)))]))

(define (reverse l) (if (null? l) null (append (reverse (cdr l)) (list (car l)))))

(define (remove x l)
  (filter (lambda (y) (not(= y x))) l))

(define (map f l)
  (if (null? l)
      null
      (list* (f (car l)) (map f (cdr l)) ) ))

(define (filter f l)
  (if (null? l)
      null
      (if (f (car l))
          (list* (car l) (filter f (cdr l)))
          (filter f (cdr l)))))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      null
      (list* (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (range start end step)
  (if (> start end)
      null
      (list* start (range (+ start step) end step))))

(define (is-palindrome l) (equal? l (reverse l)))




(define (findval to-find env) (letrec (
                     [find-e-rec (lambda (var-list)
                                   (if (null? var-list)
                                       null
                                       (let ([v (car (car var-list))])
                                         (if (string=? v to-find)
                                             (cdr (car var-list))
                                             (find-e-rec (cdr var-list))))))])
              (find-e-rec env)))




