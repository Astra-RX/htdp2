;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;e489.
(define (add-to-each n l)
  (map (lambda (i) (+ n i)) l))

;e490.
;[List-of Number] -> [List-of Number]
(define (translate lod)
  (cond [(empty? lod) '()]
        [else (local ((define accumulated
                        (map (lambda (i) (+ i (first lod))) (translate (rest lod)))))
                (cons (first lod) accumulated))]))

; fn = fn-1 + n-1, fn   - fn-1 = n-1
;                  fn-1 - fn-2 = n-2
;                     ...
;                  f2   - f1   = 2
;                         f1   = 1
; fn = sum(1, ... n-1) = n(n-1) / 2

;e491.
(define (inverse* l)
  (cond [(empty? l) '()]
        [else (append (inverse* (rest l)) (list (first l)))]))
(check-expect (inverse* '(1 2 3 4 5)) '(5 4 3 2 1))

;e492.

;e493.
;invert(x) = add-as-last(x) + invert(x-1)
;
;add-as-last(x) = 1 + add-as-last(x-1)
;
;add-as-last(n) = O(n)
;
;invert(n) = n(n+1)/2

;e494.
;...

;======================
(define (invert.v2 alox0)
  (local (; [List-of X] ??? -> [List-of X]
          ; constructs the reverse of alox
          ; accumulator ...
          (define (invert/a alox a)
            (cond
              [(empty? alox) a]
              [else
               (invert/a (rest alox) (cons (first alox) a))])))
    (invert/a alox0 '())))

;(map (lambda (l) (time (invert.v2 l)))
;     (map (lambda (i) (build-list i +)) (build-list 7 (lambda (x) (* x 1000)))))
;(map (lambda (l) (time (inverse* l)))
;     (map (lambda (i) (build-list i +)) (build-list 7 (lambda (x) (* x 1000)))))
;======================

;e495.
(define (sum.v1 alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum.v1 (rest alon)))]))

(define (sum.v2 alon0)
  (local (; [List-of Number] ??? -> Number
          ; computes the sum of the numbers on alon
          ; accumulator ...
          )
    (sum/a alon0 0)))

(define (sum/a alon a)
  (cond
    [(empty? alon) a]
    [else (sum/a (rest alon)
                 (+ (first alon) a))]))

(sum.v1 '(10 4))
;==
(+ 10 (sum.v1 '(4)))
;==
(+ 10 (+ 4 (sum.v1 '())))
;==
(+ 10 (+ 4 (+ 0)))
(+ 10 4)
;==
14
 

(sum.v2 '(10 4))
;==
(sum/a '(10 4) 0)
;==
(sum/a '(4) (+ 10 0))
;==
(sum/a '() (+ 4 10))
;==
14

;e496.

;e497.
(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))

(define (!.v2 n0)
  (local (; N N -> N
          ; computes (* n (- n 1) (- n 2) ... 1)
          ; accumulator a is the product of the 
          ; natural numbers in the interval [n0,n)
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n) (* n a))])))
    (!/a n0 1)))

;(time (foldr (lambda (i r) (!.v1 20)) 0 (build-list 100000 +)))
;(time (foldr (lambda (i r) (!.v2 20)) 0 (build-list 100000 +)))

;e498.
(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

; Tree N N -> N
; measures the height of abt
; accumulator s is the number of steps 
; it takes to reach abt from abt0
; accumulator m is the maximal height of
; the part of abt0 that is to the left of abt
;...

;e499.
(define (product l0)
  (local (;accumulator a is the product of [l0, l)
          (define (product/a l a)
            (cond [(empty? l) a]
                  [else (product/a (rest l) (* (first l) a))])))
    (product/a l0 1)))
(check-expect (product '(1 2 3 4 5)) 120)

;e500.
(define (how-many l0)
  (local (;accumulator a is the count of [l0, l)
          (define (how-many/a l a)
            (cond [(empty? l) a]
                  [else (how-many/a (rest l) (+ a 1))])))
    (how-many/a l0 0)))
(check-expect (how-many '(1 2 3 4 5)) 5)
;O(1) space

;e501.
; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n0)
  (local (;accumulator a is the sum of (+ pi (- n0 n))
          (define (add-to-pi/a n a)
            (cond
              [(zero? n) a]
              [else (add-to-pi/a (sub1 n) (add1 a))])))
    (add-to-pi/a n0 pi)))

;e502.
; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect (palindrome (explode "abc")) (explode "abcba"))
(define (palindrome s0)
  (local (;accumulator a is the mirror sequence of [s0, s)
          (define (palindrome/a s a)
            (cond [(empty? s) (append s0 (rest a))]
                  [else (palindrome/a (rest s) (cons (first s) a))])))
    (palindrome/a s0 '())))

;e503.
(check-expect (rotate.v2 '((0 4 5)
                           (1 2 3)
                           (6 7 8)
                           (0 9 8)))
              '((1 2 3)
                (6 7 8)
                (0 9 8)
                (0 4 5)))
(check-expect (rotate.v2 '((0 4 5)
                           (0 9 8)
                           (1 2 3)
                           (6 7 8)
                           (0 9 9)))
              (list (list 1 2 3)
                    (list 6 7 8)
                    (list 0 9 9)
                    (list 0 4 5)
                    (list 0 9 8)))

; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(define (rotate.v1 M)
  (cond
    [(not (= (first (first M)) 0)) M]
    [else
     (rotate.v1 (append (rest M) (list (first M))))]))

(define (rotate.v2 M0)
  (local (; Matrix Row -> Matrix 
          ; accumulator is the first 0 coefficient rows from [M0, M)
          (define (rotate/a M seen)
            (cond
              [(empty? M) seen]
              [else (if (= 0 (first (first M)))
                        (rotate/a (rest M) (cons (first M) seen))
                        (append (list (first M)) (rest M) (invert.v2 seen)))])))
    (rotate/a M0 '())))

(define (create-n-row n)
  (append (foldr (lambda (r m) (cons `(0 ,r ,r) m)) '() (build-list (sub1 n) +))
          `((,n ,n ,n))))

;e504.
;[List-of Number] -> Number
(define (to10 l0)
  (local (;Accumulator a is current number from [l0, l)
          (define (to10/a l a)
            (cond [(empty? l) a]
                  [else (to10/a (rest l) (+ (first l) (* 10 a)))])))
    (to10/a l0 0)))
(check-expect (to10 '(1 2 3 4 5)) 12345)

;e505.
; N [>=1] -> Boolean
; determines whether n is a prime number
(define (is-prime? n)
  (local (;Accumulator a is the number n
          (define (is-prime/a n a)
            (cond
              [(= n 1) #t]
              [else (if (= 0 (modulo a n))
                        #f
                        (is-prime/a (sub1 n) a))])))
    (is-prime/a (sub1 n) n)))
(check-expect (is-prime? 299) #f)

;e506.
(define (m*p f l)
  (local ((define (map/a f l a)
            (cond [(empty? l) (reverse a)]
                  [else (map/a f (rest l) (cons (f (first l)) a))])))
    (map/a f l '())))
(check-expect (m*p add1 '(1 2 3 4)) (map add1 '(1 2 3 4)))

;e507.
(define (build-l*st n0 f)
  (local (;a is the built list ranges (n, n0]
          (define (build-list/a n f a)
            (cond [(= n 0) (cons 0 a)]
                  [else (build-list/a (sub1 n) f (cons (f n) a))])))
    (build-list/a (sub1 n0) f '())))

(check-expect (build-l*st 10 +) (build-list 10 +))

;e508-509.

;e510.
;Number File File -> Boolean
;Number [List-of String] [List-of [List-of String]] -> [Void]
;w must larger than 0
(define (fmt w in-f out-f)
  (local (;Accumulator a is the length of current line
          (define (fmt/a w in-f out-f a)
            (cond [(empty? in-f) (reverse out-f)]
                  [else (if (= w a)
                            (fmt/a w in-f (cons "\n" out-f) 0)
                            (fmt/a w (rest in-f) (cons (first in-f) out-f) (add1 a)))])))
    (fmt/a w in-f out-f 0)))
(check-expect (fmt 4 '("a" "b" "c" "d" "e") '())
              (list "a" "b" "c" "d" "\n" "e"))

;e511.
;(λ (x) x) = f

;(λ (x) y) = g

;(λ (y) (λ (x) y))
;all declared

;((λ (x) x) (λ (x) x))
;all declared

;((λ (x) (x x)) (λ (x) (x x)))
;all declared

;(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w))
;all declared

;e512.
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define (is-var? any)
  (cond [(equal? 'λ any) #f]
        [(symbol? any) #t]
        [else #f]))

(check-expect (is-var? 'x) #t)
(check-expect (is-var? '(λ (x) x)) #f)
(check-expect (is-var? '()) #f)
(check-expect (is-var? 'λ) #f)
(check-expect (is-var? 66) #f)
(check-expect (is-var? "666") #f)

(define (is-λ? any)
  (and (cons? any) (equal? (first any) 'λ)))

(check-expect (is-λ? '(λ (x) x)) #t)
(check-expect (is-λ? 'x) #f)
(check-expect (is-λ? '()) #f)
(check-expect (is-λ? 'λ) #f)
(check-expect (is-λ? 66) #f)
(check-expect (is-λ? "666") #f)

(define (is-app? any)
  (and (cons? any) (= 2 (length any))))

(check-expect (is-app? '((λ (x) (x x)) (λ (x) (x x)))) #t)
(check-expect (is-app? '((λ (x) x) (λ (x) (x x)))) #t)
(check-expect (is-app? '(λ (y) (λ (x) y))) #f)
(check-expect (is-app? '(λ (x) x)) #f)
(check-expect (is-app? 'x) #f)
(check-expect (is-app? '()) #f)
(check-expect (is-app? 'λ) #f)
(check-expect (is-app? 66) #f)
(check-expect (is-app? "666") #f)

(define (λ-para lambda*)
  (first (second lambda*)))
(check-expect (λ-para '(λ (x) x)) 'x)
(define (λ-body lambda*)
  (third lambda*))

(define (app-fun app)
  (first app))
(define (app-arg app)
  (second app))

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y)) ;y is undeclared
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

(define (declareds lambda*)
  (local ()
    (cond [(symbol? lambda*) `(,lambda*)]
          [(is-λ? lambda*) (declareds (λ-body lambda*))]
          [(is-app? lambda*) (append (declareds (app-fun lambda*))
                                     (declareds (app-arg lambda*)))]
          [else #f])))

;e513.
(define-struct func [para body])
(define-struct app* [func arg])

(define ex1* (make-func 'x 'x))
(define ex2* (make-func 'x 'y))
(define ex3* (make-func 'y (make-func 'x 'y)))
(define ex4* (make-app* (make-func 'x (make-app* 'x 'x)) (make-func 'x (make-app* 'x 'x))))

;e514.
(define ex5 '((λ (x) x) (λ (y) x)))
(check-expect (undeclareds ex5)
              '((λ (x) *declared:x) (λ (y) *undeclared:x)))

;e515.
; Lam -> Lam 
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds)
                   (string->symbol (string-append "*declared:" (symbol->string le)))
                   (string->symbol (string-append "*undeclared:" (symbol->string le))))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

(define ex6 '(λ (*undeclared) ((λ (x) (x *undeclared)) y)))

;e516.
(define (undeclareds/s le0)
  (local ((define (undeclareds/a le declareds)
            (cond [(is-var? le)
                   (if (member? le declareds)
                       (string->symbol (string-append "*declared:"
                                                      (symbol->string le)))
                       (string->symbol (string-append "*undeclared:"
                                                      (symbol->string le))))]
                  [(func? le)
                   (make-func (func-para le)
                              (undeclareds/a (func-body le) (cons (func-para le) declareds)))]
                  [(app*? le)
                   (make-app* (undeclareds/a (app*-func le) declareds)
                              (undeclareds/a (app*-arg le) declareds))])))
    (undeclareds/a le0 '())))

(check-expect (undeclareds/s (make-app*
                              (make-func 'x (make-app* 'x 'x))
                              (make-app* 'x (make-app* 'x 'x))))
              (make-app*
               (make-func 'x (make-app* '*declared:x '*declared:x))
               (make-app* '*undeclared:x (make-app* '*undeclared:x '*undeclared:x))))

;e517.
(define (static-distance le0)
  (local (;Accumulator dist is the position of sym in declareds
          (define (count-distance sym declareds dist)
            (cond [(empty? declareds)
                   (string->symbol (string-append "*undeclared:" (symbol->string sym)))]
                  [(equal? (first declareds) sym) dist]
                  [else (count-distance sym (rest declareds) (add1 dist))]))
          ; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (static-distance/a le declareds)
            (cond
              [(is-var? le) (count-distance le declareds 0)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (static-distance/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (static-distance/a fun declareds)
                       (static-distance/a arg declareds)))])))
    (static-distance/a le0 '())))

(check-expect (static-distance '((λ (x) ((λ (y) (y
                                                 x))
                                         x))
                                 (λ (z) z)))
              '((λ (x) ((λ (y) (0
                                1))
                        0))
                (λ (z) 0)))
(check-expect (static-distance '((λ (x) ((λ (y) (y
                                                 (λ (w) (v
                                                         x))))
                                         x))
                                 (λ (z) z)))
              '((λ (x) ((λ (y) (0
                                (λ (w) (*undeclared:v
                                        2))))
                        0))
                (λ (z) 0)))

;e518.
;cpair ops cost constant time with no recursion or something else, our-cons only use 1 or 2 cpair ops, therefore costs only constant time.

;e519.
;Given a certain program, I can get the stat data of the occurences of cons and length, to compute the percentage of length to cons, therefore I can tell which cons performance better.
;Overall, I guess it is not acceptable because cons is used far more frequently than length, which is a O(n) function (not slow).

