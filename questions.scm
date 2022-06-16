(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (enumerate_helper s n)
    (if (null? s) s
     (cons (list n (car s)) (enumerate_helper (cdr s) (+ n 1)))))
   (enumerate_helper s 0))
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (cond
    ((null? list1) list2)
    ((null? list2) list1)
    (else
     (if
      (comp (car list1) (car list2))
      (cons (car list1) (merge comp (cdr list1) list2))
      (cons (car list2)  (merge comp (cdr list2) list1))))))
  ; END PROBLEM 16


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Problem 17

(define (nondecreaselist s)
    ; BEGIN PROBLEM 17
    (if (null? s) nil
      (let ((p (next-nondecreaselist s)))
      (cons (first p) (nondecreaselist (rest p))))))

      (define (pair a b) (lambda (c) (if c a b)))
      (define (first p) (p #t))
      (define (rest p) (p #f))

    (define (next-nondecreaselist s)
  (if (or (null? (cdr s))
  (< (car (cdr s)) (car s)))
  (pair (list (car s)) (cdr s))
  (begin
  (define p (next-nondecreaselist (cdr s)))
  (pair (cons (car s) (first p)) (rest p)))))

    ; END PROBLEM 17
