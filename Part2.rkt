;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Part2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;e129.

;e130.
;"1" is a string but 1 is a number.

;e131.
; A List-of-booleans is one of: 
; – '()
; – (cons Boolean List-of-booleans)

(define-struct pair [left right])
; A ConsPair is a structure:
;   (make-pair Any Any).
 
; Any Any -> ConsPair
(define (our-cons a-value a-list)
  (make-pair a-value a-list))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-left a-list)))

; ConsOrEmpty -> Any
; extracts the right part of the given pair
(define (our-rest a-list)
  (pair-right a-list))

;e132.
; List-of-names -> Boolean
; determines whether "Flatt" occurs on alon
(check-expect
 (contains-flatt? (cons "X" (cons "Y"  (cons "Z" '()))))
 #false)
(check-expect
 (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

;e133.
;133 is better because it gives a clear return value.

;e134.
; List-of-strings String -> Boolean
; determines whether some given string occurs on a given list of strings

(check-expect
 (contains? "ABC" (cons "X" (cons "Y"  (cons "Z" '()))))
 #false)
(check-expect
 (contains? "A" (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)

(define (contains? str alos)
  (cond
    [(empty? alos) #false]
    [(cons? alos)
     (cond [(string=? (first alos) str) #t]
           [else (contains? str (rest alos))])]))

;e135.
(contains-flatt? (cons "Flatt" (cons "C" '())))
(contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))

;e136.
(our-first (our-cons "a" '()))
(our-rest (our-cons "a" '()))

;e137.
;they both apply to the alos, the definition of alos suggests the template with 2 conditions and "(first alos)" and "(rest alos)".

;e138.
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-amounts -> Number
; sum all of the amounts of a list
(check-expect (sum (cons 1 (cons 2 (cons 3 '())))) 6)
(check-expect (sum (cons 0 (cons 1 (cons 7 (cons 8 '()))))) 16)

(define (sum aloa)
  (cond [(empty? aloa) 0]
        [(cons? aloa) (+ (first aloa) (sum (rest aloa)))]))

;e139.
; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; consumes a lon and determines whether all numbers are positive.
; List-of-numbers -> Boolean
(check-expect (pos? (cons 1 (cons 2 (cons 3 '())))) #t)
(check-expect (pos? (cons 0 (cons 1 (cons 7 (cons 8 '()))))) #f)
(check-expect (pos? (cons 3 (cons 1 (cons 7 (cons -8 '()))))) #f)

(define (pos? alon)
  (cond [(empty? alon) #t]
        [(cons? alon) (and (> (first alon) 0)
                           (pos? (rest alon)))]))

; It produces their sum if the input also belongs to List-of-amounts; otherwise it signals an error
; List-of-numbers -> Number
(check-expect (checked-sum (cons 1 (cons 2 (cons 3 '())))) 6)
(check-error (checked-sum (cons -3 (cons 1 (cons 7 (cons 8 '())))))
             "The list contains at least 1 non-positive number")

(define (checked-sum alon)
  (cond [(pos? alon) (sum alon)]
        [else (error "The list contains at least 1 non-positive number")]))

;e140.
; consumes a list of Boolean values and determines whether all of them are #true
; List-of-Boolean -> Boolean
(check-expect (all-true (cons #t (cons #t (cons #t '())))) #t)
(check-expect (all-true (cons #t (cons #t (cons #f '())))) #f)

(define (all-true alob)
  (cond [(empty? alob) #t]
        [(cons? alob) (and (first alob) (all-true (rest alob)))]))

; consumes a list of Boolean values and determines whether at least one item on the list is #true.
; List-of-Boolean -> Boolean
(check-expect (one-true (cons #t (cons #t (cons #t '())))) #t)
(check-expect (one-true (cons #t (cons #t (cons #f '())))) #t)
(check-expect (one-true (cons #f (cons #f (cons #f '())))) #f)

(define (one-true alob)
  (cond [(empty? alob) #f]
        [(cons? alob) (or (first alob) (one-true (rest alob)))]))

;e141.
; List-of-string -> String
; concatenates all strings in l into one long string
 
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
 (cat (cons "ab" (cons "cd" (cons "ef" '()))))
 "abcdef")
 
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))

;e142.
; ImageOrFalse is one of:
; – Image
; – #false

; consumes a list of images loi and a positive number n. It produces the first image on loi that is not an n by n square
; List-of-Images Number -> ImageOrFalse
(define (ill-sized? loi n)
  (cond [(empty? loi) #f]
        [(cons? loi) (if (and (= (image-width (first loi)) n)
                              (= (image-height (first loi)) n))
                         (ill-sized? (rest loi) n)
                         (first loi))]))

;e143.
(define (how-many l)
  (cond [(empty? l) 0]
        [(cons? l) (+ 1 (how-many (rest l)))]))

; List-of-temperatures -> Number
; computes the average temperature 
(define (average alot)
  (/ (sum alot) (how-many alot)))

; compute average value for a List-of-Temperatures, raise an error when empty '()
; List-of-Temperatures -> Number
(define (checked-average alot)
  (if (empty? alot) (error "cannot compute average of an empty list")
      (average alot)))

;e144.
;will do, they share the structure with List-of-temperatures

;e145.
; consumes a NEList-of-temperatures and produces #true if the temperatures are sorted in descending order
; NEList-of-temperatures -> Boolean
(check-expect (sorted>? (cons 3 (cons 2 (cons 1 '())))) #t)
(check-expect (sorted>? (cons 5 (cons 4 (cons 6 '())))) #f)
(check-expect (sorted>? (cons -5 (cons 6 '()))) #f)
(check-expect (sorted>? (cons 6 '())) #t)

(define (sorted>? alot)
  (cond [(empty? (rest alot)) #t]
        [(cons? (rest alot)) (if (> (first alot) (first (rest alot)))
                                 (sorted>? (rest alot))
                                 #f)]))

;e146.
(define (sum.e146 ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum.e146 (rest ne-l)))]))

; count number of temperatures in the list
; NEList-of-temperatures -> Number

(check-expect (how-many.e146 (cons 3 (cons 2 (cons 1 '())))) 3)
(check-expect (how-many.e146 (cons 5 (cons 4 (cons 6 '())))) 3)
(check-expect (how-many.e146 (cons -5 (cons 6 '()))) 2)
(check-expect (how-many.e146 (cons 6 '())) 1)

(define (how-many.e146 l)
  (cond [(empty? (rest l)) 1]
        [else (+ 1 (how-many.e146 (rest l)))]))

; compute average value of a NEList-of-temperatures
; NEList-of-temperatures -> Number

(check-expect (average.e146 (cons 3 (cons 2 (cons 1 '())))) 2)
(check-expect (average.e146 (cons 5 (cons 4 (cons 6 '())))) 5)
(check-expect (average.e146 (cons -5 (cons 6 '()))) 0.5)
(check-expect (average.e146 (cons 6 '())) 6)

(define (average.e146 l)
  (/ (sum.e146 l) (how-many.e146 l)))

;e147.

;e148.
;exclude empty list is better, less things to concern, simpler function purpose ensuring the 'One function per action' principle.

;e149.
; no need, for the cons accepts any value to create a list
(define (copier.v2 n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier.v2 (sub1 n) s))]))

;e150.
; N -> Number
; computes (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond [(zero? n) pi]
        [(positive? n) (+ 1 (add-to-pi (sub1 n)))]))

(check-within (add 3 pi) (+ 3 pi) 0.001)
(define (add n x)
  (cond [(zero? n) x]
        [(positive? n) (+ 1 (add (sub1 n) x))]))

;e151.
(check-expect (multiply 3 3) 9)
(check-expect (multiply 3 0) 0)
(check-expect (multiply 2 2) 4)
(define (multiply n x)
  (cond [(zero? n) 0]
        [(positive? n) (+ x (multiply (sub1 n) x))]))

;e152.
;(check-expect (row 3 (rectangle 5 5 "outline" "black")) )
(define (row n image)
  (cond [(zero? n) (empty-scene 0 0)]
        [(positive? n) (overlay/xy image (image-width image) 0
                                   (row (sub1 n) image))]))

;(check-expect (col 3 (rectangle 5 5 "outline" "black")) )
(define (col n image)
  (cond [(zero? n) (empty-scene 0 0)]
        [(positive? n) (overlay/xy image 0 (image-height image)
                                   (col (sub1 n) image))]))

;e153.
(define HALL (row 8
                  (col 18
                       (rectangle 10 10 "outline" "black"))))

(define BALLOON (circle 4 "solid" "red"))
; List-of-posns -> Image
(define (add-balloons lop)
  (cond [(empty? lop) HALL]
        [(cons? lop) (overlay/xy BALLOON
                                 (- 4 (posn-x (first lop)))
                                 (- 4 (posn-y (first lop)))
                                 (add-balloons (rest lop)))]))

;e154.

;e155.

;e156.
(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired 

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot 

(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (tock (cons 55 '())) (cons 54 '()))
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world 
; if the player presses the space bar
(check-expect (keyh '() " ") (cons HEIGHT '()))
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))

;e158.
; tells if a shot is out of canvas now
; Shot -> Boolean
(define (check-shot s)
  (if (> s 0) #t #f))

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(define (tock.e158 w)
  (cond
    [(empty? w) '()]
    [else (if (check-shot (first w))
              (cons (sub1 (first w)) (tock.e158 (rest w)))
              (tock.e158 (rest w)))]))


; ShotWorld -> ShotWorld 
(define (main.e158 w0)
  (big-bang w0
    [on-tick tock.e158]
    [on-key keyh]
    [to-draw to-image]))

;e159.

;e160.

