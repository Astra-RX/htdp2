;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;e421.
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

;non stop producing '() and dropping '()

;422.
;[List-of X] Number -> [List-of [List-of X]]
(define (list->chunks l n)
  (cond [(empty? l) '()]
        [else (append (list (take l n)) (list->chunks (drop l n) n))]))
(check-expect  (list->chunks '("a" "b" "c") 2) '(("a" "b") ("c")))

;[List-of 1String] Number -> [List-of String]
(define (bundle1 l n)
  (map implode (list->chunks l n)))
(check-expect  (bundle1 '("a" "b" "c") 2) '("ab" "c"))

;e423.
;String Number -> [List-of String]
(define (partition s n)
  (local ((define l (string-length s)))
    (cond [(equal? s "") '()]
          [else (if (< n l)
                    (cons (substring s 0 n) (partition (substring s n) n))
                    (cons (substring s 0 l) (partition (substring s l) n)))])))
(check-expect (partition "abcde" 2) '("ab" "cd" "e"))

;e424.

;QUICK-SORT
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define p (first alon)))
            (append (quick-sort< (divide alon p <)) `(,p) (quick-sort< (divide alon p >))))]))
;[List-of Number] Number [Number Number -> Boolean] -> [List-of Number]
(define (divide l p predicate)
  (cond [(empty? l) '()]
        [else (if (predicate (first l) p) (cons (first l) (divide (rest l) p predicate))
                  (divide (rest l) p predicate))]))
(check-expect (quick-sort< '(11 9 2 18 12 14 4 1)) '(1 2 4 9 11 12 14 18))

;e425.
;filter out all the number >/< than the pivot from the list

;e426.
;...
;========================
(append (append (list 7)
                (list 8)
                '())
        (list 11)
        (quick-sort< (list 14)))
;========================
(append (append (list 7)
                (list 8)
                '())
        (list 11)
        (append (quick-sort< '())
                (list 14)
                (quick-sort< '())))
;========================
(append (append (list 7)
                (list 8)
                '())
        '(11)
        (append '()
                '(14)
                '()))
;========================
(append '(7 8)
        '(11)
        '(14))
;========================
'(7 8 11 14)

(define (quick-sort<* alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define p (first alon)))
            (append (quick-sort<* (divide alon p <))
                    `(,p)
                    (quick-sort<* (divide alon p >))))]))
(quick-sort<* '(11 8 14 7))
;==========================
(append (quick-sort<* '(8 7))
        '(11)
        (quick-sort<* '(14)))
;==========================
(append (append (quick-sort<* '(7))
                '(8)
                '())
        '(11)
        (quick-sort<* '(14)))
;==========================
(append (append '(7)
                '(8)
                '())
        '(11)
        (quick-sort<* '(14)))
;==========================
(append (append '(7)
                '(8)
                '())
        '(11)
        '(14))
;==========================
'(7 8 11 14)

;e427.
(define-struct divided [small equal large])
;Generalized Quick-sort
;[X] [List-of X] Number [X X -> Number] -> [List-of X]
;Predicate [X X -> Number] returns <0 means less, 0 means equal, >0 means larger
(define (quick-sortT l th pred)
  (cond [(empty? l) '()]
        [(< (length l) th) (sort* l pred)]
        [else (local ((define pivot (first l))
                      (define divided (divide* l pivot pred)))
                (append (quick-sortT (divided-small divided) th pred)
                        (divided-equal divided)
                        (quick-sortT (divided-large divided) th pred)))]))
(check-expect (quick-sortT '(11 9 2 18 12 14 4 1) 2 asc) '(1 2 4 9 11 12 14 18))

(define (divide* l p pred)
  (local ((define small (filter (lambda (i) (< (pred i p) 0)) l))
          (define equal (filter (lambda (i) (= (pred i p) 0)) l))
          (define large (filter (lambda (i) (> (pred i p) 0)) l)))
    (make-divided small equal large)))
(check-expect (divide* '(1 3 4 2 2) 2 desc)
              (make-divided (list 3 4) (list 2 2) (list 1)))

(define (sort* l pred)
  (local ((define (insert i l)
            (cond [(empty? l) `(,i)]
                  [else (if (<= (pred i (first l)) 0)
                            (cons i l)
                            (cons (first l) (insert i (rest l))))])))
    (cond [(empty? l) '()]
          [else (insert (first l) (sort* (rest l) pred))])))
(check-expect (sort* '(4 2 1 3 1) asc) '(1 1 2 3 4))

(define asc (lambda (a b) (- a b)))
(define desc (lambda (a b) (- b a)))

;e428.
(check-expect (quick-sortT '(11 9 2 18 12 14 4 1 1 4) 2 asc)
              (list 1 1 2 4 4 9 11 12 14 18))

;e429.
;see above

;e430.
;see above


