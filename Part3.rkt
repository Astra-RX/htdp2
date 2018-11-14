;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;e235.
; [X] [List-of X] -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (equal? (first l) s)
              (contains? s (rest l)))]))

(define (contains-atom? l)
  (contains? "atom" l))
(define (contains-basic? l)
  (contains? "basic" l))
(define (contains-zoo? l)
  (contains? "zoo" l))

;e236.
; Lon -> Lon
; adds 1 to each item on l
(define (add n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ n (first l))
      (add (rest l)))]))

(define (add1* l)
  (add 1 l))
(define (plus5 l)
  (add 5 l))

;e237.

;e238.

;e239.
; [List Number Number]
(cons 1 (cons 2 '()))
; [List Number 1String]
(cons 1 (cons "str" '()))
; [List String Boolean]
(cons "str" (cons #t '()))

;e240.
; [List-of-layer CLASS] with given 
; - CLASS
; - (make-layer List-of-layer)

;[List-of-layer Number]
;[List-of-layer String]

;e241.

;e242.

;e243.
(define (f x) x)
;function is a process of computing, and this whole process can be set as a value. A value that represents a process. Besides static values, function can accept input values to produce an output.
;e244.

;e245.
(define (function=at-1.2-3-and-5.775? f g)
  (if (and (= (f 1.2) (g 1.2))
           (= (f 3) (g 3))
           (= (f -5.775) (g -5.775)))
      #t
      #f))

;e246.-e248.

;e249.
(cons f '())
(f f)
(cons f (cons 10 (cons (f 10) '())))

;e250-e253.

;e254.
; sort-n
; [List-of Number] [Number Number -> Boolean] -> [List-of Number]

; sort-s
; [List-of String] [String String -> Boolean] -> [List-of String]

; [sort CLASS]
; [CLASS] [List-of CLASS] [CLASS CLASS -> Boolean] -> [List-of CLASS]

; [sort IR]
; [List-of IR] [IR IR -> Boolean] -> [List-of IR]

;e255.

;e256.
; using [Number]
; [Number -> Number] [NEList-of Number] -> Number
; given f: (define (f x) (sqr x))
; given NEList: '(1 2 3 4 5)
; will find out 5

; similarly (argmin) will find out 1

;e257.

;e258.
; Image Polygon -> Image 
; adds an image of p to MT
(define (render-polygon img p)
  (local (; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))]))
          )
    (render-line (connect-dots img p) (first p) (last p))))
 
; Image NELoP -> Image
; connects the Posns in p in an image
(define (connect-dots img p)
  (local ((define MT (empty-scene 50 50)))
    (cond
      [(empty? (rest p)) MT]
      [else (render-line (connect-dots img (rest p))
                         (first p)
                         (second p))])))

; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

;e259.

;e260.
; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
 
; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

(list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1)
 
(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25)

;e261.
;not at all

;e262.
;create an identity Matrix with rank n
;Number -> LLN
(define (identityM n)
  (local (;Number -> Lon
          ;create 0 sequence with 1 at n-1 position, e.g. 0, 0, 0, ... 1 (at n-1)
          (define (1row n)
            (cond [(= 1 n) (list 1)]
                  [(< 1 n) (cons 0 (1row (- n 1)))]))
          ; Number Lon -> Lon
          ; add Number to the end of List
          (define (add-to-end n l)
            (cond [(empty? l) (list n)]
                  [(cons? l) (cons (first l) (add-to-end n (rest l)))]))
          ; Number LLN -> LLN
          ; add Number to the last of each list of the LLN (List of List)
          (define (add-last n lln)
            (cond [(empty? lln) '()]
                  [(cons? lln) (cons (add-to-end n (first lln)) (add-last n (rest lln)))])))
    (cond [(= 0 n) '()]
          [(< 0 n) (add-to-end (1row n) (add-last 0 (identityM (- n 1))))])))

;e263.
(inf.v2 (list 2 1 3))

;e264.
;(sup (list 2 1 3))

;e265.

;e266.

;e267.
(define USD-EURO 1.06)
; List-of-USD Number -> List-of-EURO
(define (convert-euro lous rate)
  (local ((define (convert usd)
            (* usd USD-EURO)))
    (map convert lous)))
(check-expect (convert-euro '(1) USD-EURO) '(1.06))

; List-of-Fahreiheit -> List-of-Celsius
(define (convertFC lof)
  (local ((define (f2c f)
            (* 5/9 (- f 32))))
    (map f2c lof)))
(check-expect (convertFC '(32)) '(0))

;e268.
(define-struct inventory [name description a-price s-price])
; List-of-Inventory -> List-of-Inventory
(define (sortInv loi)
  (local ((define (cmp i1 i2)
            (> (- (inventory-a-price i1) (inventory-s-price i1))
               (- (inventory-a-price i2) (inventory-s-price i2)))))
    (sort loi cmp)))
(check-expect (sortInv `(,(make-inventory "B" "ddd" 1 3)
                         ,(make-inventory "A" "ddd" 1 2)
                         ,(make-inventory "C" "ddd" 2 5)))
              `(,(make-inventory "A" "ddd" 1 2)
                ,(make-inventory "B" "ddd" 1 3)
                ,(make-inventory "C" "ddd" 2 5)))

;e269-274.

;e275-278.

;e279.
;1,3,4

;e280.
;1. 3
;2. 14
;3. 13.25

;e281.
(lambda (x) (< x 10))
(lambda (a b) (number->string (* a b)))
(lambda (n) (if (odd? n) 1 0))
(lambda (inv1 inv2) (> (inventory-s-price inv1) (inventory-s-price inv2)))
(lambda (posn bg) (overlay/xy (- 0 (posn-x posn)) (- 0 (posn-y posn)) bg))

;e282.
(define (f-plain x)
  (* 10 x))
(define f-lambda
  (lambda (x)
    (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

;e283.
(map (lambda (x) (* 10 x))
     '(1 2 3))

;e284.

;e285.
; List-of-USD Number -> List-of-EURO
(define (convert-euro-lambda lous rate)
  (map (lambda (usd) (* rate usd)) lous))
(check-expect (convert-euro-lambda '(1) USD-EURO) '(1.06))

; List-of-Fahreiheit -> List-of-Celsius
(define (convertFC-lambda lof)
  (map (lambda (f) (* 5/9 (- f 32))) lof))
(check-expect (convertFC-lambda '(32)) '(0))

;e286-e290.

;e291.
(define (map-via-fold fp l)
  l)

;e292.
; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
 
(check-expect ((sorted <) '(1 2 3)) #true)
(check-expect ((sorted <) '(2 1 3)) #false)
 
(define (sorted cmp)
  (lambda (l0)
    (local ((define (sorted/l l0)
              (cond [(empty? (rest l0)) #t]
                    [(cons? l0) (and (cmp (first l0) (first (rest l0)))
                                     (sorted/l (rest l0)))])))
      (if (empty? l0) #true (sorted/l l0)))))


;e293.
; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

; X [List-of X] -> [[List-of X] -> Boolean]
(define (found? x l)
  (lambda (l0)
    (cond [(equal? #f l0) (if (member? x l) #f #t)]
          [else (andmap (lambda (x) (member? x l)) l0)])))

(check-satisfied (find 2 '(1 2 3 "4" 5))
                 (found? 2 '(1 2 3 "4" 5)))
(check-satisfied (find 25 '("1" "2" 3 4 5))
                 (found? 25 '("1" "2" 3 4 5)))

;e294.
; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

(define (contains-l? a b)
  (andmap (lambda (a-i) (member? a-i b)) a))

(define (get n l)
  (cond [(= n 0) (if (empty? l) (error "n is larger than the length of l") (first l))]
        [(> n 0) (if (empty? l) (error "n is larger than the length of l")
                     (get (- n 1) (rest l)))]))

; X [List-of X] -> [X [List-of X] -> Boolean]
(define (is-index? x l)
  (lambda (l0)
    (cond [(equal? l0 #f) (not (member? x l))]
               [else (equal? x (get l0 l))])))

(check-satisfied (index 4 '("1" "a" 3 4 6 88)) (is-index? 4 '("1" "a" 3 4 6 88)))

;e295.

;e296.

;e297.

;e298.
; An ImageStream is a function: 
;   [N -> Image]
; interpretation a stream s denotes a series of images

; ImageStream
(define (create-rocket-scene height)
  (place-image (triangle 10 "solid" "red") 50 height (empty-scene 60 60)))

(define (my-animate s n)
  (big-bang n
      [on-tick (lambda (n) (- n 1))]
      [to-draw s]))

;e299.
;use predicate to define a set
(define (mk-set predicate)
  (lambda (n)
    (predicate n)))

(mk-set (lambda (n) (odd? n)))
(mk-set (lambda (n) (even? n)))
(mk-set (lambda (n) (= 0 (modulo n 10))))
