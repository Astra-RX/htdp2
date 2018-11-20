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
