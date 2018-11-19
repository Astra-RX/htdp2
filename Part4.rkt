;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Part4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;e310.
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)
(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

;The function consumes a family tree and counts the child structures in the tree.
;FT -> Number
(define (count-persons ft)
  (cond [(equal? ft NP) 0]
        [else (+ 1 (count-persons (child-mother ft))
                 (count-persons (child-father ft)))]))
(check-expect (count-persons Eva) 3)
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

;e311.
;It produces the average age of all child structures in the family tree.
;FT Number -> Number
(define (average-age ft year)
  (local ((define (sum-age ft year)
            (cond [(equal? ft NP) 0]
                  [else (+ (sum-age (child-mother ft) year)
                           (sum-age (child-father ft) year)
                           (- year (child-date ft)))])))
    (/ (sum-age ft year) (count-persons ft))))
(check-expect (average-age Carl 2018) 92)
(check-expect (average-age Eva 2018) 79)
(check-expect (average-age Gustav 2018) 63.8)

;e312.
;consumes a family tree and produces a list of all eye colors in the tree.
;FT -> [List-of Color]
(define (eye-colors ft)
  (cond [(equal? ft NP) '()]
        [else (append (eye-colors (child-mother ft))
                      (eye-colors (child-father ft))
                      (list (child-eyes ft)))]))
(define (pick-one l)
  (list-ref l (random (length l))))
(check-member-of (pick-one (eye-colors Carl)) "green")
(check-member-of (pick-one (eye-colors Eva)) "blue" "green" "green")
(check-member-of (pick-one (eye-colors Gustav)) "blue" "green" "green" "pink" "brown")

;e313.
; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
 
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
 
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

;check if a child has an blue-eyed ancestor
(check-expect (blue-eyed-child? Eva) #true)
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

;FT -> Boolean
(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or
      (blue-eyed-ancestor? (child-father an-ftree))
      (if (no-parent? (child-father an-ftree)) #f
          (equal? "blue" (child-eyes (child-father an-ftree))))
      (blue-eyed-ancestor? (child-mother an-ftree))
      (if (no-parent? (child-mother an-ftree)) #f
          (equal? "blue" (child-eyes (child-mother an-ftree)))))]))

;e314.
; An FF (short for family forest) is one of: 
; – '()
; – (cons FT FF)
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; New FF definition
; [FT] [List-of FT]

; FF -> Boolean
; does the forest contain any child with "blue" eyes

(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)

(define (blue-eyed-child-in-forest? a-forest)
  (foldr
   (lambda (ft result) (or (blue-eyed-child? ft)
                           result))
   #f a-forest))

(define (blue-eyed-child-in-forest?.2 a-forest)
  (ormap blue-eyed-child? a-forest))
(check-expect (blue-eyed-child-in-forest?.2 ff1) #false)
(check-expect (blue-eyed-child-in-forest?.2 ff2) #true)
(check-expect (blue-eyed-child-in-forest?.2 ff3) #true)

;e315.
;it produces the average age of all child instances in the forest
;FF Number -> Number
(define (average-age.ff ff year)
  (local ((define (sum-age ft)
            (local ()
              (cond [(equal? ft NP) 0]
                    [else (+ (sum-age (child-mother ft))
                             (sum-age (child-father ft))
                             (- year (child-date ft)))])))
          (define sum (foldr (lambda (ft sum) (+ sum (sum-age ft))) 0 ff))
          (define total-count (foldr (lambda (ft total) (+ total (count-persons ft))) 0 ff)))
    (/ sum total-count)))

(check-expect (average-age.ff ff3 2018) 76.2)

;e316.
;Any -> Boolean
(define (atom? any)
  (if (or (number? any)
          (string? any)
          (symbol? any)) #t #f))

;e317.
; An Atom is one of: 
; – Number
; – String
; – Symbol

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
  (local (; Atom Symbol -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)]))
          ; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))
 
;e318.
(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '((world) ((world) hello) hello)) 4)

;SL -> Number
;depth of a SL
(define (depth-sl sl)
  (cond [(empty? sl) 0]
        [else (max (+ 1 (depth (first sl)))
                   (depth-sl (rest sl)))]))
;S-Expr -> Number
(define (depth s)
  (cond
    [(atom? s) 1]
    [else (depth-sl s)]))

;e319.
(check-expect (substitute 'world 'world 'abcd) 'abcd)
(check-expect (substitute '(world hello) 'world 'abcd) '(abcd hello))
(check-expect (substitute '((world) ((world) hello) hello) 'world 'abcd)
              '((abcd) ((abcd) hello) hello))

;S-expr Symbol Symbol -> S-expr
(define (substitute sexp old new)
  (local (;Symbol Symbol Symbol -> Symbol
          ;if old symbol matches, return new symbol
          (define (replace cur)
            (if (equal? cur old) new cur))
          ;SL Symbol Symbol -> SL
          ;replace all old symbol with new symbol
          (define (subst-sl sl)
            (cond [(empty? sl) '()]
                  [else (cons (substitute (first sl) old new)
                              (subst-sl (rest sl)))])))
    (cond
      [(atom? sexp) (replace sexp)]
      [else (subst-sl sexp)])))

;e320.
; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – [List-of S-expr]

;S-expr Symbol -> Number
;redesigned count
(define (count.e320 sexp sym)
  (local ((define (count-atom sexp)
            (if (equal? sexp sym) 1 0))
          (define (count-list sexp)
            (foldr (lambda (sexp count)
                     (+ (count.e320 sexp sym) count))
                   0 sexp)))
    (cond [(number? sexp) 0]
          [(string? sexp) 0]
          [(symbol? sexp) (count-atom sexp)]
          [(list? sexp) (count-list sexp)])))

(check-expect (count.e320 '(((world hello) hello) 123 "abc") 'hello) 2)

;e321.
; [X]
; An S-expr is one of: 
; – X
; – [List-of S-expr]
