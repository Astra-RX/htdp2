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

;e322.
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(make-node
 15
 'd
 NONE
 (make-node
  24 'i NONE NONE))
;     15
;     /\
;    /  \
;  NONE  24
;        /\
;       /  \
;     NONE NONE

	
(make-node
 15
 'd
 (make-node
  87 'h NONE NONE)
 NONE)
;       15
;       /\
;      /  \
;     87  NONE
;     /\
;    /  \
;  NONE NONE

;e323.
;If the tree contains a node structure whose ssn field is n, the function produces the value of the name field in that node. Otherwise, the function produces #false.
;Number BT -> [Maybe Symbol]
(define (search-bt ssn bt)
  (local ((define (combine bt1 bt2)
            (if (equal? #f bt1) bt2 bt1)))
    (cond [(no-info? bt) #f]
          [else (if (equal? (node-ssn bt) ssn) (node-name bt)
                    (combine (search-bt ssn (node-left bt))
                             (search-bt ssn (node-right bt))))])))
(define bt1 (make-node 63 'a
                       (make-node 29 'b
                                  (make-node 15 'd
                                             (make-node 10 'h NONE NONE)
                                             (make-node 24 'i NONE NONE))
                                  NONE)
                       (make-node 89 'c
                                  (make-node 77 'l NONE NONE)
                                  (make-node 95 'g
                                             NONE
                                             (make-node 99 'o NONE NONE)))))
(check-expect (search-bt 77 bt1) 'l)
(check-expect (search-bt 89 bt1) 'c)

;e324.
;It consumes a binary tree and produces the sequence of all the ssn numbers in the tree as they show up from left to right when looking at a tree drawing.
;BT -> [List-of Number]
(define (inorder bt)
  (local ()
    (cond [(no-info? bt) '()]
          [else (append (inorder (node-left bt))
                        (list (node-ssn bt)) 
                        (inorder (node-right bt)))])))
(check-expect (inorder bt1) '(10 15 24 29 63 77 89 95 99))

;e325.
;The function consumes a number n and a BST. If the tree contains a node whose ssn field is n, the function produces the value of the name field in that node. Otherwise, the function produces NONE.
;Number BST -> [Symbol or NONE]
(define (search-bst ssn bst)
  (local ()
    (cond [(no-info? bst) NONE]
          [else (cond [(= ssn (node-ssn bst)) (node-name bst)]
                      [(< ssn (node-ssn bst)) (search-bst ssn (node-left bst))]
                      [(> ssn (node-ssn bst)) (search-bst ssn (node-right bst))])])))
(check-expect (search-bst 89 bt1) 'c)
(check-expect (search-bst 999 bt1) NONE)
(check-expect (search-bst 10 bt1) 'h)

;e326.
;It consumes a BST B, a number N, and a symbol S. It produces a BST that is just like B and that in place of one NONE subtree contains the node structure
;BST Number Symbol -> BST
(define (create-bst bst n sym)
  (local ((define (insert bst n sym)
            (make-node (node-ssn bst) (node-name bst)
                       (make-node n
                                  sym
                                  (node-left bst) NONE)
                       (node-right bst))))
    (cond [(no-info? bst) (make-node n sym NONE NONE)]
          [else (cond [(= n (node-ssn bst))
                       (insert bst n sym)]
                      [(< n (node-ssn bst))
                       (make-node (node-ssn bst) (node-name bst)
                                  (create-bst (node-left bst) n sym)
                                  (node-right bst))]
                      [(> n (node-ssn bst))
                       (make-node (node-ssn bst) (node-name bst)
                                  (node-left bst)
                                  (create-bst (node-right bst) n sym))])])))
(check-expect (inorder (create-bst bt1 22 'z)) '(10 15 22 24 29 63 77 89 95 99))
(check-expect (inorder (create-bst bt1 999 'z)) '(10 15 24 29 63 77 89 95 99 999))
(check-expect (inorder (create-bst bt1 10 'z)) '(10 10 15 24 29 63 77 89 95 99))

;e327.
;It consumes a list of numbers and names and produces a BST by repeatedly applying create-bst.
; [List-of [List Number Symbol]] -> BST
(define (create-bst-from-list list)
  (foldr (lambda (pair bst)
           (create-bst bst (first pair) (second pair)))
         NONE list))
;using foldr or foldl will result in different structure.
(check-expect (create-bst-from-list '((99 o)
                                      (77 l)
                                      (24 i)
                                      (10 h)
                                      (95 g)
                                      (15 d)
                                      (89 c)
                                      (29 b)
                                      (63 a)))
              bt1)

;e328.
; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute.e328 '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute.e328 '(((world bye) "hello") 42) 'bye '42)
              '(((world 42) "hello") 42))
 
(define (substitute.e328 sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else (map (lambda (s) (substitute.e328 s old new)) sexp)]))

;e329.

;e330.
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

(define fig.123.cons (cons (cons "part1" (cons "part2" (cons "part3" '())))
                           (cons "read!"
                                 (cons (cons (cons "hang" (cons "draw" '()))
                                             (cons (cons "read!" '())
                                                   '()))
                                       '()))))
(define fig.123 '(("part1" "part2" "part3")
                  "read!"
                  (("hang" "draw")
                   ("read!"))))

;e331.
;determines how many files a given Dir.v1 contains
;Dir.v1 -> Number
(define (how-many dir)
  (cond [(empty? dir) 0]
        [else (+ (if (string? (first dir))
                     1 
                     (how-many (first dir)))
                 (how-many (rest dir)))]))
(check-expect (how-many fig.123) 7)

;e332.
(define-struct dir.v2 [name content])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

(define fig.123.v2 (make-dir.v2 "TS"
                                (list (make-dir.v2 "Text"
                                                   (list "part1" "part2" "part3"))
                                      "read!"
                                      (make-dir.v2 "Libs"
                                                   (list (make-dir.v2 "Code"
                                                                      (list "hang" "draw"))
                                                         (make-dir.v2 "Docs"
                                                                      (list "read!" )))))))

;e333.
(check-expect (how-many.v2 fig.123.v2) 7)
(define (how-many.v2 dir)
  (local ((define (how-many-lofd lofd)
            (cond [(empty? lofd) 0]
                  [else (+ (cond [(string? (first lofd)) 1]
                                 [(dir.v2? (first lofd)) (how-many.v2 (first lofd))])
                           (how-many-lofd (rest lofd)))])))
    (how-many-lofd (dir.v2-content dir))))

;e334.
;modify dir structure
;(define-struct dir [name content size attr])

;e335.
;(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

(define fig.123.v3
  (make-dir "TS"
            (list (make-dir "Text"
                            (list)
                            (list (make-file "part1" 99 "")
                                  (make-file "part2" 52 "")
                                  (make-file "part3" 17 "")))
                  (make-dir "Libs"
                            (list (make-dir "Code"
                                            (list)
                                            (list (make-file "hang" 8 "")
                                                  (make-file "draw" 2 "")))
                                  (make-dir "Docs"
                                            (list)
                                            (list (make-file "read!" 19 ""))))
                            (list)))
            (list (make-file "read!" 10 ""))))

;e336.
;determines how many files a given Dir.v3 contains.
;Dir.v3 -> Number
(define (how-many.v3 dir)
  (local ((define (how-many-dir* dir*)
            (foldr (lambda (dir count)
                     (+ count (how-many.v3 dir))) 0 dir*)))
    (;(dir-name dir)
     + (how-many-dir* (dir-dirs dir))
       (length (dir-files dir)))))
(check-expect (how-many.v3 fig.123.v3) 7)

;e337.
;[Dir.v3 File.v3]
; A Dir.v3 is a structure: 
;   (make-dir.v3 String [List-of Dir.v3] [List-of File])

(require htdp/dir)

;e338.
(define W (create-dir "z:\\artoria\\codex"))
(define (how-many.e338 dir)
  (local ((define (how-many-dir* dir*)
            (foldr (lambda (dir count)
                     (+ count (how-many.e338 dir))) 0 dir*)))
    (;(dir.v3-name dir)
     + (how-many-dir* (dir-dirs dir))
       (length (dir-files dir)))))
(how-many.e338 W)

;e339.
;consumes a Dir and a file name and determines whether or not a file with this name occurs in the directory tree.
;Dir String -> Boolean
(define (find? dir name)
  (local ((define (find-dir* dir*)
            (foldr (lambda (dir result)
                     (or result (find? dir name))) #f dir*))
          (define (find-files* files*)
            (foldr (lambda (file result)
                     (if (equal? name (file-name file))
                         #t
                         (or #f result)))
                   #f files*)))
    ;(dir-name dir)
    (or (find-dir* (dir-dirs dir))
        (find-files* (dir-files dir)))))
(check-expect (find? W "Part3.rkt") #t)

;e340.
;Dir -> [List-of Dir/File]
(define (ls dir)
  (append (map dir-name (dir-dirs dir))
          (map file-name (dir-files dir))))
(ls W)

;e341.
;Dir -> Number
(define (du dir)
  (+ 1
     (foldr (lambda (dir total) (+ (du dir) total)) 0 (dir-dirs dir))
     (foldr (lambda (file total) (+ (file-size file) total)) 0 (dir-files dir))))
(du W)

;e342.
; A Path is [List-of String].
; interpretation directions into a directory tree

;If (find? d f) is #true, find produces a path to a file with name f; otherwise it produces #false.
;Dir string -> Path/#false
(define (find dir name)
  (local (;[List-of Dir] -> Path/#f
          (define (find-dirs* dir*)
            (cond [(empty? dir*) #f]
                  [else (if (find? (first dir*) name)
                            (find (first dir*) name)
                            (find-dirs* (rest dir*)))]))
          ;[List-of File] -> Path/#f
          (define (find-files* files*)
            (cond [(empty? files*) #f]
                  [else (if (equal? (file-name (first files*))
                                    name)
                            (list (file-name (first files*)))
                            (find-files* (rest files*)))]))
          ;Path/#f -> Path/#f
          (define (append-cur-dirname path)
            (if (boolean? path) #f (append (list (dir-name dir)) path)))
          (define cur-file (find-files* (dir-files dir))))
    (if (boolean? cur-file)
        (append-cur-dirname (find-dirs* (dir-dirs dir))) ; search sub-dir
        (append-cur-dirname cur-file))))                 ; found in cur-dir


;Dir String -> [List-of Path]
(define (find-all dir name)
  (local (;[List-of File] -> Path/[List-of String]
          (define (find-files files*)
            (cond [(empty? files*) '()]
                  [else (if (equal? (file-name (first files*)) name)
                            (list (file-name (first files*)))
                            (find-files (rest files*)))]))
          ;[List-of Dir] -> [List-of Path]
          (define (find-dirs dirs)
            (foldr append '() (map (lambda (dir) (find-all dir name)) dirs)))
          ;local var
          (define file-path (find-files (dir-files dir)))
          (define sub-path (find-dirs (dir-dirs dir)))
          ;append Path as prefix to every on in the list
          ;[List-of Path] String -> [List-of Path]
          (define (append-cur-dir prefix lop)
            (map (lambda (path) (append (list prefix) path)) lop)))
    (cond [(empty? file-path)
           ;append prefix for paths in sub-dir
           (append-cur-dir (dir-name dir) sub-path)]
          [else
           ;append file path in current dir and sub-dir
           (append (append-cur-dir (dir-name dir) sub-path)    
                   (list (append (list (dir-name dir)) file-path)))])))

(check-expect (find-all fig.123.v3 "read!")
              (list (list "TS" "Libs" "Docs" "read!")
                    (list "TS" "read!")))
;(find-all W "HEAD")

;e343.
;Dir -> [List-of Path]
(define (ls-R dir)
  (append (map (lambda (file) (list (dir-name dir) (file-name file))) (dir-files dir))
          (map (lambda (path) (append (list (dir-name dir)) path))
               (foldr append '() (map ls-R (dir-dirs dir))))))
(check-expect (ls-R fig.123.v3)
              (list
               (list "TS" "read!")
               (list "TS" "Text" "part1")
               (list "TS" "Text" "part2")
               (list "TS" "Text" "part3")
               (list "TS" "Libs" "Code" "hang")
               (list "TS" "Libs" "Code" "draw")
               (list "TS" "Libs" "Docs" "read!")))

;e344.
;Dir name -> [List-of Path]
(define (find-all.ls dir name)
  (filter (lambda (path) (member? name path)) (ls-R dir)))
(check-expect (find-all.ls fig.123.v3 "read!")
              (list (list "TS" "read!")
                    (list "TS" "Libs" "Docs" "read!")))
(check-expect (find-all.ls fig.123.v3 "Docs")
              (list (list "TS" "Libs" "Docs" "read!")))

;e345.
(define-struct add [left right])
(define-struct mul [left right])

;BSL-expr definition
; - (make-add sub-BSL sub-BSL)
; - (make-nul sub-BSL sub-BSL)

;sub-BSL
; - Number
; - BSL-expr

(make-add 10 -10)
(make-add (make-mul 20 3) 33)
(make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9)))

(+ -1 -2)
(+ (* -2 -3) 33)
(* (+ 1 (* 2 3)) 3.14)

;e346.
;BSL-value
; - Number

;BSL-expr
; - BSL-value
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

;e347.
(define (eval-expression bsl)
  (cond [(number? bsl) bsl]
        [(add? bsl) (+ (eval-expression (add-left bsl)) (eval-expression (add-right bsl)))]
        [(mul? bsl) (* (eval-expression (mul-left bsl)) (eval-expression (mul-right bsl)))]))
(check-expect (eval-expression (make-add 10 -10)) 0)
(check-expect (eval-expression (make-add (make-mul -2 -3) 33)) 39)

;e348.
;BSL-Bool
; - Boolean

(define-struct and* [list*])
(define-struct or* [list*])
(define-struct not* [b])
;BSL-bool-expr
; - BSL-Bool
; - (make-and [List-of BSL-Bool])
; - (make-or [List-of BSL-Bool])
; - (make-not [BSL-Bool])

(define (eval-bool-expression bsl-b)
  (cond [(boolean? bsl-b) bsl-b]
        [(and*? bsl-b) (foldr (lambda (b result)
                                (and (eval-bool-expression b) result))
                              #t (and*-list* bsl-b))]
        [(or*? bsl-b) (foldr (lambda (b result)
                               (or (eval-bool-expression b) result))
                             #f (or*-list* bsl-b))]
        [(not*? bsl-b) (not (not*-b bsl-b))]))
(check-expect (eval-bool-expression (make-and* (list (make-and* (list #t #f))
                                                     (make-or* (list #f #f))
                                                     #t))) #f)
(check-expect (eval-bool-expression (make-not* #f)) #t)

;e349.
; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
(check-expect (parse '(+ (* 2 3) 4)) (make-add (make-mul 2 3) 4))

(define WRONG "WRONG")
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
(check-expect (parse-sl '(+ 3 4)) (make-add 3 4))
(check-expect (parse-sl '(* 3 4)) (make-mul 3 4))
(check-error (parse-sl '(* 3 4 5)) WRONG)
(check-error (parse-sl '(* 3)) WRONG)
(check-error (parse-sl '(= 3 4)) WRONG)
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
(check-expect (parse-atom 49) 49)
(check-error (parse-atom "atom") WRONG)
(check-error (parse-atom 'symbol) WRONG)

;e350.

;e351.
;S-expr -> BSL-value
(define (interpreter-expr sexp)
  (eval-expression (parse sexp)))

(check-expect (interpreter-expr '(+ (* 2 3) 4)) 10)
(check-error (parse '(+ (* 2 3 4) 4)) WRONG)

;e352.
; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define (subst bsl-v sym n)
  (cond [(add? bsl-v) (make-add (subst (add-left bsl-v) sym n)
                                (subst (add-right bsl-v) sym n))]
        [(mul? bsl-v) (make-mul (subst (mul-left bsl-v) sym n)
                                (subst (mul-right bsl-v) sym n))]
        [(equal? bsl-v sym) n]
        [else bsl-v]))
(check-expect (subst (make-add 4 (make-mul 'x 5)) 'x 5) (make-add 4 (make-mul 5 5)))

;e353.
;BSL-var-expr -> Boolean
(define (numeric? bsl-v)
  (cond [(number? bsl-v) #t]
        [(symbol? bsl-v) #f]
        [(add? bsl-v) (and (numeric? (add-left bsl-v)) (numeric? (add-right bsl-v)))]
        [(mul? bsl-v) (and (numeric? (mul-left bsl-v)) (numeric? (mul-right bsl-v)))]))

(check-expect (numeric? (make-add 4 (make-mul 'x 5))) #f)

;e354.
;The checked function consumes a BSL-var-expr and determines its value if numeric? yields true for the input. Otherwise it signals an error.
;BSL-var-expr -> Boolean
(define (eval-variable bsl)
  (cond [(numeric? bsl) (eval-expression bsl)]
        [else (error WRONG)]))
(check-error (eval-variable (make-add 4 (make-mul 'x 5))) WRONG)
(check-expect (eval-variable (make-add 4 (make-mul 4 5))) 24)

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

;BSL-var-expr AL -> BSL-value
(define (eval-variable* ex da)
  (local ((define final (foldr (lambda (a ex)
                                 (subst ex (first a) (second a))) ex da)))
    (if (numeric? final) (eval-expression final)
        (error WRONG))))
(check-expect (eval-variable* (make-add 4 (make-mul 'x 5))
                              (list (list 'x 5)))
              29)
(check-expect (eval-variable* (make-add 4 (make-mul 'x 'y))
                              (list (list 'x 5)
                                    (list 'y 6)))
              34)
(check-error (eval-variable* (make-add 4 (make-mul 'x 'y))
                             (list (list 'x 5)))
             WRONG)

;e355.
; BSL-var-expr AL -> Number
(define (eval-var-lookup e da)
  (cond [(number? e) e]
        [(symbol? e) (local ((define found (assq e da)))
                       (if (equal? found #f) (error WRONG)
                           (second found)))]
        [(add? e) (+ (eval-var-lookup (add-left e) da)
                     (eval-var-lookup (add-right e) da))]
        [(mul? e) (* (eval-var-lookup (mul-left e) da)
                     (eval-var-lookup (mul-right e) da))]))

(check-expect (eval-var-lookup (make-add 4 (make-mul 'x 'y))
                               (list (list 'x 5)
                                     (list 'y 6)))
              34)
(check-error (eval-var-lookup (make-add 4 (make-mul 'x 'y))
                              (list (list 'x 5)))
             WRONG)

;e356.
(define-struct function-call [name arg])

;BSL with function
; - Number
; - Symbol
; - (make-function-call Symbol Symbol)
; - (make-add BSL BSL)
; - (make-mul BSL BSL)

(make-function-call 'k (make-add 1 1))
(make-mul 5 (make-function-call 'k (make-add 1 1)))
(make-mul (make-function-call 'i 5) (make-function-call 'k (make-add 1 1)))

;e357.
(define (eval-definition1 ex f x b)
  (cond [(number? ex) ex]
        [(symbol? ex) (error WRONG)]
        [(function-call? ex) (if (equal? f (function-call-name ex))
                                 (local ((define arg (function-call-arg ex))
                                         (define value (eval-definition1 arg f x b))
                                         (define plugd (subst b x value)))
                                   (eval-definition1 plugd f x b))
                                 (error WRONG))]
        [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                      (eval-definition1 (add-right ex) f x b))]
        [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                      (eval-definition1 (mul-right ex) f x b))]))
(check-expect (eval-definition1 (make-function-call 'k (make-add 1 (make-function-call 'k 1)))
                                'k
                                'x
                                (make-add 'x 'x)) 6)
(check-expect (eval-definition1 (make-function-call 'k (make-add 1 1))
                                'k
                                'x
                                (make-mul 'x 'x)) 4)
;(eval-definition1 (make-function-call 'f 5) 'f 'x (make-function-call 'f 3))

;e358.
(define-struct BSL-fun-def [name arg body])
(define f (make-BSL-fun-def 'f 'x (make-add 3 'x)))
(define g (make-BSL-fun-def 'g 'y (make-function-call 'f (make-mul 2 'y))))
(define h (make-BSL-fun-def 'h 'v (make-add (make-function-call 'f 'v)
                                            (make-function-call 'g 'v))))

;BSL-fun-def*
; [List-of BSL-fun-def]
(define da-fgh (list (make-BSL-fun-def 'f 'x (make-add 3 'x))
                     (make-BSL-fun-def 'g 'y (make-function-call 'f (make-mul 2 'y)))
                     (make-BSL-fun-def 'h 'v (make-add (make-function-call 'f 'v)
                                                       (make-function-call 'g 'v)))))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(define (lookup-def da f)
  (local ((define found (filter (lambda (fun) (equal? f (BSL-fun-def-name fun))) da)))
    (if (empty? found) (error WRONG)
        (first found))))

;e359.
;BSL-fun-expr FAL BSL-fun-def* -> BSL-value
(define (eval-function* ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (error WRONG)]
        [(function-call? ex)
         (local ((define found
                   (lookup-def da (function-call-name ex))))
           (if (equal? found #f) (error WRONG)
               (local ((define arg (function-call-arg ex))
                       (define value (eval-function* arg da))
                       (define plugd (subst-fun (BSL-fun-def-body found)
                                                (BSL-fun-def-arg found)
                                                value)))
                 (eval-function* plugd da))))]
        [(add? ex) (+ (eval-function* (add-left ex) da)
                      (eval-function* (add-right ex) da))]
        [(mul? ex) (* (eval-function* (mul-left ex) da)
                      (eval-function* (mul-right ex) da))]))
(define (subst-fun ex sym n)
  (cond [(add? ex) (make-add (subst-fun (add-left ex) sym n)
                             (subst-fun (add-right ex) sym n))]
        [(mul? ex) (make-mul (subst-fun (mul-left ex) sym n)
                             (subst-fun (mul-right ex) sym n))]
        [(function-call? ex)
         (if (equal? sym (function-call-arg ex))
             (make-function-call (function-call-name ex) n)
             (make-function-call (function-call-name ex)
                                 (subst-fun (function-call-arg ex) sym n)))]
        [(equal? ex sym) n]
        [else ex]))
(check-expect (subst-fun (BSL-fun-def-body h) 'v 5)
              (make-add (make-function-call 'f 5) (make-function-call 'g 5)))
(check-expect (eval-function* (make-function-call 'h 5) da-fgh) 21)
(check-error (eval-function* (make-function-call 'a 5) da-fgh) WRONG)

;e360.
; BSL-da-all
; [list-of BSL-fun-def/BSL-var-def]
; - '()
; - (cons BSL-fun-def BSL-da-all)
; - (cons BSL-var-def BSL-da-all)

(define-struct BSL-var-def [name expr])
(define da-all (list (make-BSL-var-def 'a 5)
                     (make-BSL-var-def 'b 5)
                     (make-BSL-fun-def 'f 'x (make-add 3 'x))
                     (make-BSL-fun-def 'g 'y (make-function-call 'f (make-mul 2 'y)))
                     (make-BSL-fun-def 'h 'v (make-add (make-function-call 'f 'v)
                                                       (make-function-call 'g 'v)))))

; BSL-da-all Symbol -> BSL-var-def/Error
(define (lookup-con-def da x)
  (cond [(empty? da) (error WRONG)]
        [(BSL-var-def? (first da)) (if (equal? x (BSL-var-def-name (first da)))
                                       (first da)
                                       (lookup-con-def (rest da) x))]
        [else (lookup-con-def (rest da) x)]))
(check-error (lookup-con-def da-all 'e) WRONG)
(check-error (lookup-con-def da-all 'f) WRONG)
(check-expect (lookup-con-def da-all 'a) (make-BSL-var-def 'a 5))

(define (lookup-fun-def da x)
  (local ((define found (filter (lambda (def)
                                  (if (and (BSL-fun-def? def)
                                           (equal? x (BSL-fun-def-name def)))
                                      #t #f)) da)))
    (if (empty? found) (error WRONG)
        (first found))))
(check-error (lookup-fun-def da-all 'e) WRONG)
(check-error (lookup-fun-def da-all 'a) WRONG)
(check-expect (lookup-fun-def da-all 'f) (make-BSL-fun-def 'f 'x (make-add 3 'x)))

;e361.
;BSL-fun-expr BSL-da-all -> BSL-value
(define (eval-all ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (local ((define found (lookup-con-def da ex)))
                        (BSL-var-def-expr found))]
        [(function-call? ex)
         (local ((define found (lookup-fun-def da (function-call-name ex)))
                 (define arg (function-call-arg ex))
                 (define value (eval-all arg da))
                 (define plugd (subst-fun (BSL-fun-def-body found)
                                          (BSL-fun-def-arg found)
                                          value)))
           (eval-all plugd da))]
        [(add? ex) (+ (eval-all (add-left ex) da)
                      (eval-all (add-right ex) da))]
        [(mul? ex) (* (eval-all (mul-left ex) da)
                      (eval-all (mul-right ex) da))]))

(check-expect (eval-all (make-function-call 'h (make-function-call 'h 'a)) da-all) 69)
(check-error (eval-all (make-function-call 'h 'c) da-all) WRONG)

;e362.
;S-expr Sl -> BSL-value
(define (interpreter sexp sl)
  (local ((define ex (parse-ex sexp))
          (define da (parse-Sl sl)))
    (eval-all ex da)))
;function as value not implemented
(check-expect (interpreter '(h 5) '((define (h x) (* x x)))) 25)
(check-expect (interpreter '(h (h h)) '((define (h x) (* x x))
                                        (define h 5)))
              625)

;S-expr -> BSL-fun-expr
(define (parse-ex s)
  (local (; Atom -> BSL-expr 
          (define (parse-atom s)
            (cond [(number? s) s]
                  [(string? s) (error WRONG)]
                  [(symbol? s) s]))
          ; SL -> BSL-fun-expr 
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond [(= L 1) (error WRONG)]
                    [(and (= L 2) (symbol? (first s)))
                     (make-function-call (first s) (parse-ex (second s)))]
                    [(and (= L 3) (symbol? (first s)))
                     (cond [(symbol=? (first s) '+)
                            (make-add (parse-ex (second s)) (parse-ex (third s)))]
                           [(symbol=? (first s) '*)
                            (make-mul (parse-ex (second s)) (parse-ex (third s)))]
                           [else (error WRONG)])]
                    [else (error WRONG)]))))
    (cond [(atom? s) (parse-atom s)]
          [else (parse-sl s)])))

(check-expect (parse-ex '(h (h a)))
              (make-function-call 'h (make-function-call 'h 'a)))
(check-expect (parse-ex '(+ (* 2 3) (h (h a))))
              (make-add (make-mul 2 3) (make-function-call 'h (make-function-call 'h 'a))))

;S-expr
; - Atom
; - SL

;SL
; - '()
; - (cons S SL)

;[List-of S-expr] -> BSL-da-all
(define (parse-Sl l)
  (map (lambda (def)
         (local ((define L (length def)))
           (cond [(and (equal? 'define (first def))
                       (= L 3))
                  (cond [(symbol? (second def))
                         (make-BSL-var-def (second def) (parse-ex (third def)))]
                        [(list? (second def))
                         (make-BSL-fun-def (first (second def))
                                           (second (second def))
                                           (parse-ex (third def)))])]
                 [else (error WRONG)]))) l))

(check-expect (parse-Sl '((define (h v) (+ 3 5))
                          (define a 5)))
              (list (make-BSL-fun-def 'h 'v (make-add 3 5)) (make-BSL-var-def 'a 5)))

;e363-386.

;e387.
;[List-of Symbol] [List-of Number] -> [List-of '(Symbol Number)]
(define (cross l1 l2)
  (local (;[List-of Symbol] Number -> [List-of '(Symbol Number)]
          (define (one-cross l item)
            (cond [(empty? l) '()]
                  [else (append (list (list (first l) item))
                                (one-cross (rest l) item))])))
    (foldr (lambda (item2 base)
             (append (one-cross l1 item2) base)) '() l2)))
(check-expect (cross '(a b c) '(1 2)) '((a 1) (b 1) (c 1) (a 2) (b 2) (c 2)))

;e388.

;e389.
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)
;[List-of String] [List-of String] -> [List-of PhoneRecord]
(define (zip lon lop)
  (cond [(empty? lon) '()]
        [else (cons (make-phone-record (first lon) (first lop))
                    (zip (rest lon) (rest lop)))]))
(check-expect (zip '("a" "b" "c") '("1" "2" "3"))
              `(,(make-phone-record "a" "1")
                ,(make-phone-record "b" "2")
                ,(make-phone-record "c" "3")))


;e390.
(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right

; A list of Directions is also called a path.
; - '()
; - (cons Direction LoD)

;TOS [List-of Direction] -> Symbol
(define (tree-pick t p)
  (cond [(and (symbol? t) (empty? p)) t]
        [(and (symbol? t) (cons? p)) (error "no more branches")]
        [(and (branch? t) (empty? p)) (error "no more directions")]
        [(and (branch? t) (cons? p))
         (cond [(equal? (first p) 'left) (tree-pick (branch-left t) (rest p))] 
               [(equal? (first p) 'right) (tree-pick (branch-right t) (rest p))])]))

;        __B__
;       /     \
;      B       B
;     / \     / \
;    B   'c  'd  'e
;   / \
;  'a  'b
(define tree.e390 (make-branch (make-branch (make-branch 'a 'b)
                                            'c)
                               (make-branch 'd 'e)))

(define path.e390 '(left left left))

(check-expect (tree-pick tree.e390 path.e390) 'a)
(check-expect (tree-pick tree.e390 '(left right)) 'c)
(check-expect (tree-pick tree.e390 '(right left)) 'd)
(check-error (tree-pick tree.e390 '(right left left)) "no more branches")
(check-error (tree-pick tree.e390 '(left)) "no more directions")

;e391.

;e392.

;e393.
; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s
;Son.R Son.R -> Son.R
(define (union a b)
  (cond [(empty? a) b]
        [else (if (member? (first a) b)
                  (union (rest a ) b)
                  (cons (first a) (union (rest a) b)))]))

(check-expect (union '(a b) '(b c)) '(a b c))
(check-expect (union '(a b) '()) '(a b))
(check-expect (union '() '(a b)) '(a b))

;e394-411.
