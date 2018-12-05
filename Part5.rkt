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

;e431.
;bundle
;1. empty string, or string length is less than chunk size.
;2. stay empty or same.
;3. generate first chunk, recur on the remaining string (shorter)
;4. append first chunk in front of the result of the rest recursions.

;e432.

;e433.
(define (bundle-checked s n)
  (if (> n 0) (bundle s n) (error "cannot produce meaningful result when n <= 0")))

;e434.
;the size of the problem stays the same in some cases, therefore the sort will be an infinite loop.

;e435.
(define (quick-sort** l)
  (cond [(empty? l) '()]
        [(= (length l) 1) l]
        [else (local ((define pivot (first l)))
                (append (quick-sort** (smallers (rest l) pivot))
                        `(,pivot)
                        (quick-sort** (largers (rest l) pivot))))]))
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (<= (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

;e436.

;e437.
(define (special P solve combine-solutions)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
      P
      (special (rest P) solve combine-solutions))]))

(define (solve.1 P)
  0)
(define (combine-solutions.1 P Q)
  (+ 1 Q))
(check-expect (special '(1 2 3 5 4) solve.1 combine-solutions.1) 5)

(define (solve.2 P)
  '())
(define (combine-solutions.2 P Q)
  (cons (- 0 (first P)) Q))
(check-expect (special '(1 2 3 5 4) solve.2 combine-solutions.2) '(-1 -2 -3 -5 -4))

(define (solve.3 P)
  '())
(define (combine-solutions.3 P Q)
  (cons (string-upcase (first P)) Q))
(check-expect (special '("AA" "bb" "cc") solve.3 combine-solutions.3) '("AA" "BB" "CC"))

;e438.
;1. check trivial case, gcd is 1, otherwise, pick the smaller n of both, test n, n-1, n-2 ... whether is the gcd of (m,n).

;e439.
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))
;(time (gcd-structural 101135853 45014640))

;e440.
(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S)) 
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))
(time (gcd-generative 101135853 45014640))

;e441.
(quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
;==
(append (quick-sort< '(6 8 9 3 2))
        '(10)
        (quick-sort< '(14 12 11 14 16)))
;==
(append (append (quick-sort< '(3 2))
                '(6)
                (quick-sort< '(8 9)))
        '(10)
        (append (quick-sort< '(12 11))
                '(14)
                (quick-sort< '(16))))
;==
(append (append (append (quick-sort< '(2))
                        '(3)
                        (quick-sort< '()))
                '(6)
                (append (quick-sort< '())
                        '(8)
                        (quick-sort< '(9))))
        '(10)
        (append (append (quick-sort< '(11))
                        '(12)
                        (quick-sort< '())))
        '(14)
        (append (quick-sort< '())
                '(16)
                (quick-sort< '())))
;==
(append (append (append (append (quick-sort< '())
                                '(2)
                                (quick-sort< '()))
                        '(3)
                        (quick-sort< '()))
                '(6)
                (append (quick-sort< '())
                        '(8)
                        (append (quick-sort< '())
                                '(9)
                                (quick-sort< '()))))
        '(10)
        (append (append (quick-sort< '(11))
                        '(12)
                        (quick-sort< '())))
        '(14)
        (append (quick-sort< '())
                '(16)
                (quick-sort< '())))
; 8 recursive appends

(quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;==
(append (quick-sort< '())
        `(1)
        (quick-sort< '(2 3 4 5 6 7 8 9 10 11 12 13 14)))
;==
(append (quick-sort< '())
        `(1)
        (append (quick-sort< '())
                '(2)
                (quick-sort< '(3 4 5 6 7 8 9 10 11 12 13 14))))
;==
(append (quick-sort< '())
        `(1)
        (append (quick-sort< '())
                '(2)
                (append (quick-sort< '())
                        '(3)
                        (quick-sort< '(4 5 6 7 8 9 10 11 12 13 14)))))
;==
(append (quick-sort< '())
        `(1)
        (append (quick-sort< '())
                '(2)
                (append (quick-sort< '())
                        '(3)
                        (append (quick-sort< '())
                                '(4)
                                (quick-sort< '(5 6 7 8 9 10 11 12 13 14))))))
;==
(append (quick-sort< '())
        `(1)
        (append (quick-sort< '())
                '(2)
                (append (quick-sort< '())
                        '(3)
                        (append (quick-sort< '())
                                '(4)
                                (append (quick-sort< '())
                                        '(5)
                                        (quick-sort< '(6 7 8 9 10 11 12 13 14)))))))
;==
;...
;==
(append
 (quick-sort< '())
 `(1)
 (append
  (quick-sort< '())
  '(2)
  (append
   (quick-sort< '())
   '(3)
   (append
    (quick-sort< '())
    '(4)
    (append
     (quick-sort< '())
     '(5)
     (append
      (quick-sort< '())
      '(6)
      (append
       (quick-sort< '())
       '(7)
       (append
        (quick-sort< '())
        '(8)
        (append
         (quick-sort< '())
         '(9)
         (append
          (quick-sort< '())
          '(10)
          (append
           (quick-sort< '())
           '(11)
           (append
            (quick-sort< '())
            '(12)
            (append
             (quick-sort< '())
             '(13)
             (append
              (quick-sort< '())
              '(14)
              '()))))))))))))))
;13 recursive appends

;e442.
(time (quick-sort< (build-list 1000 (lambda (n) (if (> n 0) (random n) 0)))))
(time (quick-sortT (build-list 1000 (lambda (n) (if (> n 0) (random n) 0))) 100 asc))
(time (sort* (build-list 1000 (lambda (n) (if (> n 0) (random n) 0))) asc))

;e443.
;It produces new irrelevant problem when solving it.

;e444.
;When computing gcd there's no need to produce the divisor larger than S, which is the upper bound of the divisor list.

;e445.

;e446.
(define ε 0.000001)
; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption 
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
          (find-root f left mid)]
         [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
          (find-root f mid right)]))]))

;e447.
(define (poly x)
  (* (- x 2) (- x 4)))
(time (find-root poly 0 6))

;e448.

;e449.
(define (find-root* f left right)
  (local ((define (find-root-lr f left right fl fr)
            (local ((define mid (/ (+ left right) 2))
                    (define f@mid (f mid))
                    (define fl (f left))
                    (define fr (f right)))
              (cond
                [(<= (- right left) ε) left]
                [else (cond [(or (<= fl 0 f@mid) (<= f@mid 0 fl))
                             (find-root-lr f left mid fl f@mid)]
                            [(or (<= f@mid 0 fr) (<= fr 0 f@mid))
                             (find-root-lr f mid right f@mid fr)])]))))
    (find-root-lr f left right (f left) (f right))))

;e450.
(define (find-root-mono f left right)
  (local ((define (find-root-lr f left right fl fr)
            (local ((define mid (/ (+ left right) 2))
                    (define f@mid (f mid))
                    (define fl (f left))
                    (define fr (f right)))
              (cond
                [(<= (- right left) ε) left]
                [else (cond [(< (* fl f@mid) 0)
                             (find-root-lr f left mid fl f@mid)]
                            [(< (* fr f@mid) 0)
                             (find-root-lr f mid right f@mid fr)])]))))
    (find-root-lr f left right (f left) (f right))))

;e451.

;e452.
;first-line: get the first line of the file
;remove-first-line: remove the first line of the file

;e453.
;[Token]
;[1String/String] with no whitespace

;Line -> [List-of Token]
(define (tokenize l)
  (local ((define (get-word l)
            (cond [(empty? l) '()]
                  [else (if (string-whitespace? (first l))
                            '()
                            (cons (first l) (get-word (rest l))))]))
          (define (remove-word l)
            (cond [(empty? l) '()]
                  [else (if (string-whitespace? (first l))
                            (rest l)
                            (remove-word (rest l)))]))
          (define (split-word l)
            (cond [(empty? l) '()]
                  [else (cons (get-word l) (split-word (remove-word l)))])))
    (map implode (split-word l))))

(check-expect (tokenize '("a" "b" "c" " " "d" "e" "\n" "f" "g" "h" "\n"))
              '("abc" "de" "fgh"))

;e454.
;Number [List-of Number] -> [List-of [List-of Number]]
(define (create-matrix n l)
  (local ((define (first-n n l)
            (cond [(empty? l) '()]
                  [else (if (> n 0)
                            (cons (first l) (first-n (sub1 n) (rest l)))
                            '())]))
          (define (remove-n n l)
            (cond [(empty? l) '()]
                  [else (if (> n 0)
                            (remove-n (sub1 n) (rest l))
                            l)])))
    (cond [(empty? l) '()]
          [else (cons (first-n n l) (create-matrix n (remove-n n l)))])))
(check-expect
 (create-matrix 2 (list 1 2 3 4))
 (list (list 1 2)
       (list 3 4)))
(check-expect
 (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
 (list (list 1 2 3)
       (list 4 5 6)
       (list 7 8 9)))

;e455.
(define (slope f r)
  (/ (- (f (+ r ε)) (f (- r ε))) (* 2 ε)))
(check-expect (slope (lambda (x) (* x x)) 1) 2)

;e456.
(define (root-of-tangent f r)
  (- r (/ (f r) (slope f r))))

;e457.
;Number -> Number 
(define (double-amount ir)
  (local ((define (income ir n)
            (cond [(>= (expt ir n) 2) n]
                  [else (income ir (add1 n))])))
    (income ir 1)))

;e458.
(define (integrate-kepler f a b)
  (* 1/2 (- b a) (+ (f a) (f b))))
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε.inte)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε.inte)

;e459.
(define R 1000)
(define ε.inte 0.01)
;[Number -> Number] Number Number -> Number
(define (integrate-rectangles f a b)
  (local ((define step (/ (- b a) R))
          (define seq (map (lambda (x) (+ a (* (+ 1/2 x) step))) (build-list R +)))
          (define fseq (map f seq))
          ;[List-of Number] [List-of Number] -> Number
          (define (mul-sum seq fseq)
            (cond [(empty? seq) 0]
                  [else (+ (* (first seq) (first fseq)) (mul-sum (rest seq) (rest fseq)))])))
    (foldr + 0 (map (lambda (fx) (* fx step)) fseq))))

(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ε.inte)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ε.inte)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε.inte)

;e460.
(define (integrate-dc f a b)
  (cond [(< (- b a) ε.inte) (integrate-kepler f a b)]
        [else (local ((define mid (/ (+ b a) 2)))
                (+ (integrate-dc f a mid)
                   (integrate-dc f mid b)))]))
(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ε.inte)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ε.inte)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε.inte)

;e461.
(define (integrate-adaptive f a b)
  (local ((define mid (/ (+ b a) 2))
          (define ab (integrate-kepler f a b))
          (define amid (integrate-kepler f a mid))
          (define midb (integrate-kepler f mid b)))
    (cond [(< (abs (- ab amid midb)) (* (- b a) ε.inte 0.1)) ab]
          [else (+ (integrate-adaptive f a mid)
                   (integrate-adaptive f mid b))])))
(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 ε.inte)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 ε.inte)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε.inte)

;e462-470.

;e471.
(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

;Node Graph -> [List-of Node]
(define (neighbors n g)
  (local ((define connections (filter (lambda (p) (equal? (first p) n)) g)))
    (cond [(empty? connections) '()]
          [else (rest (first connections))])))
(check-expect (neighbors 'A sample-graph) '(B E))

;e472.
; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))
(check-expect (find-path 'A 'G sample-graph) (list 'A 'B 'E 'F 'G))

;Graph -> Boolean
(define (test-on-all-nodes G)
  (cond [(empty? G) #t]
        [else (if (test-one (first (first G)) (nodes G) G)
                  (test-on-all-nodes (rest G))
                  #f)]))
;Node [List-of Node] Graph -> Boolean
(define (test-one n l G)
  (cond [(empty? l) #t]
        [else (local ((define side1 (find-path n (first l) G))
                      (define side2 (find-path (first l) n G)))
                (if (and (boolean? side1) (boolean? side2)) #f
                    (test-one n (rest l) G)))]))
(define (nodes G)
  (map (lambda (c) (first c)) G))
(check-expect (test-one 'B (nodes sample-graph) sample-graph) #t)
(check-expect (test-one 'F (nodes sample-graph) sample-graph) #f)
(check-expect (test-on-all-nodes sample-graph) #f)

(define cyclic-graph
  '((A B E)
    (B E F)
    (C B D)
    (D)
    (E C F)
    (F D G)
    (G)))

;e473.

;e474.
;must use 2 functions for 2 recursion on list and neighbors

;e475.
(define (find-path/list* lo-Os D G)
  (local ((define candidates (filter (lambda (can) (not (boolean? can)))
                                     (map (lambda (o)
                                            (local ((define candidate (find-path o D G)))
                                              (cond [(boolean? candidate) #f]
                                                    [else candidate]))) lo-Os))))
    (if (empty? candidates) #f (first candidates))))

;e476.
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
(define (fsm-match? an-fsm a-string)
  (local ((define los (explode a-string)))
    (cond [(empty? los) #f]
          [else (local ((define current (first los))
                        (define next (accept? an-fsm (fsm-initial an-fsm) current)))
                  (cond [(boolean? next) #f]
                        [(equal? next (fsm-final an-fsm)) (if (empty? (rest los)) #t #f)]
                        [else (fsm-match? (make-fsm next
                                                    (fsm-transitions an-fsm)
                                                    (fsm-final an-fsm))
                                          (implode (rest los)))]))])))
(check-expect (fsm-match? fsm-a-bc*-d "abbd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "abbcc") #f)
(check-expect (fsm-match? fsm-a-bc*-d "dabbcc") #f)
(check-expect (fsm-match? fsm-a-bc*-d "abbdcc") #f)

;FSM State 1String -> [Maybe State]
(define (accept? fsm init s)
  (local ((define match (filter (lambda (t) (and (equal? init (transition-current t))
                                                 (equal? s (transition-key t))))
                                (fsm-transitions fsm))))
    (if (and (equal? init (fsm-initial fsm))
             (cons? match))
        (transition-next (first match))
        #f)))
(check-expect (accept? fsm-a-bc*-d "AA" "a") "BC")
(check-expect (accept? fsm-a-bc*-d "BC" "a") #f)

(define fsm-a-bc*-d-e*-f
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD")
         (make-transition "DD" "e" "DD")
         (make-transition "DD" "f" "FF"))
   "FF"))
(check-expect (fsm-match? fsm-a-bc*-d-e*-f "abbdcc") #f)
(check-expect (fsm-match? fsm-a-bc*-d-e*-f "bbdcc") #f)
(check-expect (fsm-match? fsm-a-bc*-d-e*-f "adf") #t)
(check-expect (fsm-match? fsm-a-bc*-d-e*-f "abccbdeef") #t)

;e477.
; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
     (foldr (lambda (item others)
              (local ((define without-item
                        (arrangements (remove item w)))
                      (define add-item-to-front
                        (map (lambda (a) (cons item a))
                             without-item)))
                (append add-item-to-front others)))
            '()
            w)]))
 
(define (all-words-from-rat? w)
  (and (member (explode "rat") w)
       (member (explode "art") w)
       (member (explode "tar") w)))
 
(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)

;trivial case is '(), which produces '(())
;to generate next problem, use (foldr) to pick one from w and (add-item-to-front) to the arrangements of the rest to create all arrangements
;once create all of the arrangments, collect them into the '(others) list, continue on next pick in w

;e478.
;Two diagonals of a 2x3 square are 2 valid configuration, which can be placed in 3x3 chess board by 4 different types.

;e479.
(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

;QP QP -> Boolean
(define (threatening? qp1 qp2)
  (local ((define (test-row qp1 qp2)
            (equal? (posn-y qp1) (posn-y qp2)))
          (define (test-col qp1 qp2)
            (equal? (posn-x qp1) (posn-x qp2)))
          (define (test-diag qp1 qp2)
            (equal? (abs (- (posn-x qp1) (posn-x qp2)))
                    (abs (- (posn-y qp1) (posn-y qp2))))))
    (or (test-row qp1 qp2)
        (test-col qp1 qp2)
        (test-diag qp1 qp2))))

(check-expect (threatening? (make-posn 2 1) (make-posn 3 2)) #t)
(check-expect (threatening? (make-posn 2 1) (make-posn 1 2)) #t)
(check-expect (threatening? (make-posn 0 0) (make-posn 3 3)) #t)
(check-expect (threatening? (make-posn 4 0) (make-posn 2 2)) #t)
(check-expect (threatening? (make-posn 1 0) (make-posn 2 2)) #f)

;e480.
(define QUEEN (text "♛" 19 "blue"))
(define GRID-SIZE 19)
(define (board-size n)
  (+ (* n (+ GRID-SIZE 1)) 1))
(define (create-board n)
  (foldr (lambda (i board)
           (overlay/xy (rectangle (board-size n) 1 "solid" "black")
                       0 (- 0 (* i (+ GRID-SIZE 1)))
                       board))
         (foldr (lambda (i board)
                  (overlay/xy (rectangle 1 (board-size n) "solid" "black")
                              (- 0 (* i (+ GRID-SIZE 1)))
                              0 board))
                (rectangle (board-size n) (board-size n) "outline" "black")
                (build-list (- n 1) add1))
         (build-list (- n 1) add1)))

;N [List-of QP] Image -> Image
(define (render-queens n lp image)
  (local ((define (render-queen qp board)
            (overlay/xy image
                        (- (* -1 (posn-x qp) (+ GRID-SIZE 1)) 1)
                        (- (* -1 (posn-y qp) (+ GRID-SIZE 1)) 1) board)))
    (foldr (lambda (q board)
             (render-queen q board)) (create-board n) lp)))

;e481.
;Number -> [[list-of QP] -> Boolean]
(define (n-queens-solution? n)
  (lambda (lqp)
    (and (= (length lqp) n)
         (not (threatening?* lqp)))))
(check-satisfied 4QUEEN-SOLUTION-1 (n-queens-solution? 4))
(check-satisfied 4QUEEN-SOLUTION-2 (n-queens-solution? 4))

(define (threatening?* lqp)
  (cond [(empty? lqp) #f]
        [else (or (foldr (lambda (qp result)
                           (threatening? (first lqp) qp)) #f (rest lqp))
                  (threatening?* (rest lqp)))]))
(check-expect (threatening?* 4QUEEN-SOLUTION-2) #f)

(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
(define 4QUEEN-SOLUTION-1
  (list  (make-posn 1 0) (make-posn 3 1)
         (make-posn 0 2) (make-posn 2 3)))

;SET SET -> Boolean
(define (set=? a b) #t)

;e482+483.

;1.trivial case:         place 0 queen on the board.
;2.trivial result:       return the board as-is.
;3.generate new problem: place n-th queen on the board, investigate all the 
;                        next possible places for queen n, generates a problem
;                        for each case, deal with the case, return a QP.
;4.combine solutions:    add QP to the board.

;N -> [List-of QP]
;create all QPs of a nxn board
(define (create-nxn n)
  (foldr (lambda (ith board)
           (append (foldr (lambda (jth row)
                            (cons (make-posn ith jth) row))
                          '() (build-list n +))
                   board))
         '() (build-list n +)))

;I
;a Board collects those positions where a queen can still be placed;
;A Board is a list of QPs and size N
;[[List-of QP] N] 
(define-struct board-I [space size])
; N -> Board 
; creates the initial n by n board
(define (board0-I n)
  (make-board-I (create-nxn n) n))
(check-expect (board0-I 2) (make-board-I (list
                                          (make-posn 0 0)
                                          (make-posn 0 1)
                                          (make-posn 1 0)
                                          (make-posn 1 1)) 2))
 
; Board QP -> [Maybe Board]
; places a queen at qp on a-board
(define (add-queen-I a-board qp)
  (make-board-I
   (filter (lambda (open) (not (threatening? qp open))) (board-I-space a-board))
   (board-I-size a-board)))
(check-expect (add-queen-I (board0-I 2) (make-posn 0 1)) (make-board-I '() 2))
(check-expect (add-queen-I (board0-I 0) (make-posn 0 1)) (make-board-I '() 0))
(check-expect (add-queen-I (board0-I 3) (make-posn 0 0))
              (make-board-I (list (make-posn 1 2) (make-posn 2 1)) 3))
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots-I a-board)
  (board-I-space a-board))
(check-expect (find-open-spots-I (board0-I 2)) (create-nxn 2))
(check-expect (find-open-spots-I (board0-I 0)) '())

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens-I a-board n)
  (cond [(= n 0) (board-I-space a-board)]
        [else (local ((define next-spot (find-open-spots-I a-board)))
                (cond [(empty? next-spot) #f]
                      [else (local ((define candidate (investigate-I next-spot a-board n)))
                              (if (boolean? candidate) #f
                                  candidate))]))]))
;[List-of QP] Board N -> [Maybe [List-of QP]]
;gives the n-th queen solution, QP or #false if no solution
(define (investigate-I spots board n)
  (local ((define queens
            (filter cons?
                    (map (lambda (spot)
                           (place-queens-I (add-queen-I board spot) (sub1 n))) spots))))
    (if (empty? queens) #f (first queens))))

(define (n-queens-I n)
  (render-queens n (place-queens-I (board0-I n) n) QUEEN))
;NOT WORKING

;II.
;a Board contains the list of positions where a queen has been placed;
;A Board is a list of QPs and size N
;[[List-of QP] N]
(define-struct board-II [queens size])

; N -> Board 
; creates the initial n by n board
(define (board0-II n)
  (make-board-II '() n))
 
; Board QP -> Board
; places a queen at qp on a-board
(define (add-queen-II a-board qp)
  (make-board-II (cons qp (board-II-queens a-board)) (board-II-size a-board)))
(check-expect (add-queen-II (make-board-II '() 2) (make-posn 0 1))
              (make-board-II (list (make-posn 0 1)) 2))
(check-expect (add-queen-II (make-board-II (list (make-posn 0 2)) 3) (make-posn 0 1))
              (make-board-II (list (make-posn 0 1) (make-posn 0 2)) 3))
 
; Board Number -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots-II a-board)
  (foldr (lambda (q result)
           (filter (lambda (qp) (not (threatening? q qp))) result))
         (create-nxn (board-II-size a-board)) (board-II-queens a-board)))
(check-expect (find-open-spots-II (board0-II 2))
              (create-nxn 2))
(check-expect (find-open-spots-II (make-board-II (list (make-posn 0 0)) 2))
              '())
(check-expect (find-open-spots-II (make-board-II (list (make-posn 0 0)) 3))
              (list (make-posn 1 2) (make-posn 2 1)))

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens-II a-board n)
  (cond [(= n 0) (board-II-queens a-board)]
        [else (local ((define next-spot (find-open-spots-II a-board)))
                (cond [(empty? next-spot) #f]
                      [else (local ((define candidate (investigate-II next-spot a-board n)))
                              (if (boolean? candidate) #f
                                  candidate))]))]))
;[List-of QP] Board N -> [Maybe [List-of QP]]
;gives the n-th queen solution, QP or #false if no solution
(define (investigate-II spots board n)
  (local ((define queens
            (filter cons?
                    (map (lambda (spot)
                           (place-queens-II (add-queen-II board spot) (sub1 n))) spots))))
    (if (empty? queens) #f (first queens))))

(define (n-queens-II n)
  (render-queens n (place-queens-II (board0-II n) n) QUEEN))

;III.
;a Board is a grid of n by n squares, each possibly occupied by a queen. Use a structure with three fields to represent a square: one for x, one for y, and a third one saying whether the square is threatened.
;Square is a structure with
(define-struct SQ [x y threatened])
;A Board is a list of squares
;[List-of Square]
