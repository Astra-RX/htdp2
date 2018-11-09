;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Part2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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

;e161.
; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))

(define PAYMENT-PER-HOUR 12)
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* PAYMENT-PER-HOUR h))

(check-expect (wage* (cons 4 (cons 8 (cons 20 '()))))
              (cons 48 (cons 96 (cons 240 '()))))

;e162.
; check if working hours is valid
; Number -> Boolean
(define (whrs-protect hrs)
  (if (> hrs 100) #f
      #t))

;e163.
; List-of-fahrenheit -> List-of-celsius
(define (convertFC lof)
  '())

; Number -> Number
(define (f2c f)
  0)

;e164.
; List-of-euros -> List-of-usds
(define (convert-euro loe)
  '())

; Number -> Number
(define (euro-usd euro rate)
  (* euro rate))

(define (conver-euro* loe rate)
  '())

;e165.
;List-of-strings -> List-of-strings
(define (subst-robot los)
  (cond [(empty? los) '()]
        [(cons? los) (cons (robot-r2d2 (first los)) (subst-robot (rest los)))]))

;String -> String
(define (robot-r2d2 str)
  (if (string=? str "robot") "r2d2" str))

;String String String -> String
(define (replace str old new)
  (if (string=? str old) new str))

;List-of-strings String String -> List-of-strings
(define (substitute los old new)
  (cond [(empty? los) '()]
        [(cons? los) (cons (replace (first los) old new)
                           (substitute (rest los) old new))]))

;e166.
; Paycheck structure
(define-struct paycheck [name amount])
; A paycheck contains the employee's name and wage amount.
; (make-paycheck name wage) to create a paycheck.

;e167.

;e168.

;e169.

;e170.

;e171.
;A List-of-strings is one of below
; - '()
; - (cons String List-of-strings)

;A List-of-list-of-strings is one of below
; - '()
; - (cons List-of-strings List-of-list-of-strings)

;e172.
; Los -> String
(define (collapse-ln los)
  (cond [(empty? los) ""]
        [(cons? los) (string-append (first los)
                                    " "
                                    (collapse-ln (rest los)))]))

; LLS -> String
(define (collapse lls)
  (cond [(empty? lls) ""]
        [(cons? lls) (string-append (collapse-ln (first lls))
                                    "\n"
                                    (collapse (rest lls)))]))
;(write-file "ttt.dat" (collapse (read-words/line "ttt.txt")))

;e173.
; Los -> Los
(define (no-articles ln)
  (cond [(empty? ln) ln]
        [(cons? ln) (if (or (string=? (first ln) "a")
                            (string=? (first ln) "an")
                            (string=? (first ln) "the"))
                        (cons (no-articles (rest ln)))
                        (cons (first ln) (no-articles (rest ln))))]))

;e174.
; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

;encode a word into a number string
;List-of-letters -> String
(check-expect (encode-word (explode "z a")) "122032097")
(define (encode-word lol)
  (cond [(empty? lol) ""]
        [else (string-append (encode-letter (first lol))
                             (encode-word (rest lol)))]))

;encode a list of string into a numeric string
;Los -> String
(define (encode-ln ln)
  (cond [(empty? ln) ""]
        [(cons? ln) (string-append (encode-word (explode (first ln)))
                                   (encode-ln (rest ln)))]))

;encode LLS into a numeric string.
;LLS -> String
(define (encode-lls lls)
  (cond [(empty? lls) ""]
        [(cons? lls) (string-append (encode-ln (first lls))
                                    (encode-lls (rest lls)))]))

;String -> String
(define (encode-file file)
  (encode-lls (read-words/line file)))

(check-expect (encode-file "ttt.txt")
              "084084084080117116117112105110097112108097099101119104101114101105116039115101097115121116111115101101116104101099114121112116105099097100109111110105115104109101110116084046084046084046087104101110121111117102101101108104111119100101112114101115115105110103108121115108111119108121121111117099108105109098044105116039115119101108108116111114101109101109098101114116104097116084104105110103115084097107101084105109101046080105101116072101105110")

;e175.

;e176.
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

;non-recursive operation, cannot be implemented

;e177.-e180.

;e181.-e185.

;e186.
; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(check-satisfied (sort> (list 9 8 17 6 15 4 3 12 1 10)) sorted>?)

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

(check-satisfied (sort>/bad (list 9 8 17 6 15 4 3 12 1 10)) sorted>?)
;(check-expect (sum (sort>/bad (list 9 8 17 6 15 4 3 12 1 10)))
;              (sum (list 9 8 17 6 15 4 3 12 1 10)))

;e187.
(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

; List-of-gp -> List-of-gp
; sort list of gp
(define (sort-gp l)
  (cond [(empty? l) '()]
        [(cons? l) (insert-gp (first l) (sort-gp (rest l)))]))

; GP List-of-gp -> List-of-gp
(define (insert-gp gp l)
  (cond [(empty? l) (list gp)]
        [(cons? l) (if (compare-gp gp (first l))
                       (cons gp l)
                       (cons (first l) (insert-gp gp (rest l))))]))

; GP GP -> Boolean
(define (compare-gp gp1 gp2)
  (if (> (gp-score gp1) (gp-score gp2)) #t #f))
(check-expect (compare-gp (make-gp "B" 22) (make-gp "C" 123)) #f)

(make-gp "A" 12)
(make-gp "B" 22)
(make-gp "C" 123)

(check-expect (sort-gp (list (make-gp "B" 22)
                             (make-gp "A" 12)
                             (make-gp "C" 123)))
              (list (make-gp "C" 123)
                    (make-gp "B" 22)
                    (make-gp "A" 12)))

;e188.

;e189.
; alon is sorted descended
(define (search-sorted n alon)
  (cond [(empty? alon) #f]
        [(cons? alon) (cond [(= n (first alon)) #t]
                            [(> n (first alon)) #f]
                            [else (search-sorted n (rest alon))])]))
;(search-sorted 5.5 (list 9 8 7 6 5 4 3 2 1 0))

;e190.

;e191.
; a plain background image 
(define MT (empty-scene 50 50))
(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

(check-expect
 (render-polygon MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-line BG p1 p2)
  (scene+line BG (posn-x p1) (posn-y p1) (posn-x p2) (posn-y p2) "red"))

(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else
     (render-line
      (connect-dots img (rest p))
      (first p)
      (second p))]))

; Image Polygon -> Image 
; adds an image of p to img
(define (render-polygon img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))

(define (last p)
  (cond
    [(empty? (rest p)) (first p)]
    [else (last (rest p))]))

;e192.
;they share the same list definition

;e193.
(define (render-poly-f img p)
  (connect-dots img (cons (last p) p)))
(check-expect
 (render-poly-f MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-poly-l img p)
  (connect-dots img (add-to-end (first p) p)))
(define (add-to-end item l)
  (cond [(empty? l) (list item)]
        [(cons? l) (cons (first l) (add-to-end item (rest l)))]))
(check-expect
 (render-poly-l MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

;e194.
(define (connect-dots+ img p posn)
  (cond
    [(empty? (rest p)) (render-line img (first p) posn)]
    [else
     (render-line
      (connect-dots+ img (rest p) posn)
      (first p)
      (second p))]))

(define (render-poly+ img p)
  (connect-dots+ img p (first p)))
(check-expect
 (render-poly+ MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

