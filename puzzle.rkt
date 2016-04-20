;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;*********************************************
;;  Duc Le
;;  CS 135 Fall 2015
;;*********************************************
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;; Aside: 
;; Taken from Module 8
;; A nested list of X (Nest-List-X) is one of:
;; * empty
;; * (cons X Nest-List-X)
;; * (cons Nest-List-X Nest-List-X)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz02 (read-puzzle "puzzle02.txt"))
(define puzz03 (read-puzzle "puzzle03.txt"))
(define puzz04 (read-puzzle "puzzle04.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define my-state
  (make-state '((#\# #\# #\#) (#\# #\. #\.) (#\# #\. #\.))
              (list (make-wpos 0 0 true 3)
                    (make-wpos 0 0 false 3)) '("CAT" "CAR")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))
(check-expect (flip (make-wpos 0 0 false 2))
              (make-wpos 0 0 true 2))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;; Tests:
(check-expect (flip (make-wpos 5 0 true 3))
              (make-wpos 0 5 false 3))
(check-expect (flip (make-wpos 50 50 true 50))
              (make-wpos 50 50 false 50))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose g) produces the transposed grid, i.e. row numbers and col numbers
;; are swapped. 
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose '((#\A #\B #\C) (#\D #\E #\F) (#\X #\Y #\Z)))
              '((#\A #\D #\X) (#\B #\E #\Y) (#\C #\F #\Z)))

(define (transpose g)
  (local [;; (create-list g count) produces a list with the first of each loc in
          ;; g according to count
          ;; create-list: Grid Nat -> (listof Char) 
          (define (create-list g count)
            (cond [(empty? g) empty]
                  [else (cons (list-ref (first g) count)
                              (create-list (rest g) count))]))
          ;; (create g count n-rows) produces the transposed grid.
          ;; cons the result of create-list onto the recursive result of the
          ;; rest to do so.
          ;; create: Grid Nat Nat -> Grid 
          (define (create g count n-rows)
            (cond
              [(= count n-rows) empty]
              [else (cons (create-list g count)
                          (create g (add1 count) n-rows))]))]
    (cond [(empty? g) empty]
          [else (create g 0 (length (first g)))])))

;; Tests:
(check-expect (transpose '((#\A #\B) (#\C #\D) (#\E #\F)))
              '((#\A #\C #\E) (#\B #\D #\F)))
(check-expect (transpose empty) empty)
(check-expect (transpose '(())) empty)


;; (find-wpos loc row) produces a list of all horizontal WPos that occur
;; in the given row. 
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))
(check-expect (find-wpos (string->list "#####") 4)
              (list (make-wpos 4 0 true 5)))

(define (find-wpos loc row)
  (local [;; (len loc) produces the the length of word.
          ;; len: (listof Char) -> Nat
          (define (len loc)
            (cond [(or (empty? loc) (char=? #\. (first loc))) 0]
                  [else (add1 (len (rest loc)))]))
          ;; (removefn loc len) removes elements at the start of the list
          ;; based on the value of len.
          ;; removefn: (listof Char) Nat -> (listof Char) 
          (define (removefn loc len)
            (cond [(= len 0) loc]
                  [else (removefn (rest loc) (sub1 len))]))
          ;; (wpos loc row count) produces a list of all horiztonal WPos that
          ;; occur in the given row, using a seeded count value.
          ;; wpos: (listof Char) Nat Nat -> (listof WPos) 
          (define (wpos loc row count)
            (cond [(empty? loc) empty]
                  [(and (cons? (rest loc)) (char=? #\# (first loc))
                        (char=? #\#(second loc)))
                   (cons (make-wpos row count true (len loc))
                         (wpos (removefn loc (len loc))
                               row (+ (len loc) count)))]
                  [else (wpos (rest loc) row (add1 count))]))]
    (wpos loc row 0)))

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)
(check-expect (find-wpos empty 3) empty)


;; (initial-state puzzle) produces the initial State of puzzle.
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(check-expect (initial-state puzz02)
              (make-state (map string->list (first puzz02))
                          (list (make-wpos 1 6 true 4) (make-wpos 3 5 true 6)
                                (make-wpos 7 3 true 4) (make-wpos 9 0 true 4)
                                (make-wpos 7 3 false 3) (make-wpos 3 6 false 6)
                                (make-wpos 0 8 false 5))
                          (second puzz02)))

(define (initial-state puzzle)
  (local [;; -------------------------------------------------------------------
          ;; (horizontal g n) produces a list of list of horizontal WPos to be
          ;; flattened later. The list of horizontal WPos is due to the
          ;; top level function: find-wpos
          ;; horizontal: Grid Nat -> (listof (listof WPos))  
          (define (horizontal g n)
            (cond [(empty? g) empty]
                  [else (cons (find-wpos (first g) n)
                              (horizontal (rest g) (add1 n)))]))
          ;; (vertical g n) produces a list of list of vertical WPos to be
          ;; flattened later. The list of vertical WPos is due to the usage of
          ;; mapping the top level function: flip
          ;; onto the result of the top level function: find-wpos
          (define (vertical g n)
            (cond [(empty? g) empty]
                  [else (cons (map flip (find-wpos (first g) n))
                              (vertical (rest g) (add1 n)))]))
          ;; ------------------------------------------------------------------
          ;; (grid los) produces a grid from the given los.
          ;; grid: (listof Str) -> (listof Char)
          (define (grid puzzle)
            (map string->list puzzle))
          ;; (flatten lst) produces the flatten list from given lst
          ;; flatten: Nest-List-WPos -> (listof WPos)
          (define (flatten lst)
            (foldr append empty lst))
          ;; (dostuff puzzle) produces the inital State of puzzle by appending
          ;; the list of horizontal and vertical WPos, flattening, and filtering
          ;; dostuff: Puzzle -> Puzzle 
          (define (dostuff puzzle)
            (filter wpos? (flatten (append (horizontal (grid (first puzzle)) 0)
                                           (vertical (transpose
                                                      (grid (first puzzle))) 0))
                                   )))]
    ;; -------------------------------------------------------------------------
    (make-state (grid (first puzzle)) (dostuff puzzle) (second puzzle))))

;; Tests:
(check-expect
 (initial-state puzz03) (make-state
                         (map string->list
                              (list "#####" "#####" "#####" "#####" "#####"))
                         (list (make-wpos 0 0 true 5) (make-wpos 1 0 true 5)
                               (make-wpos 2 0 true 5) (make-wpos 3 0 true 5)
                               (make-wpos 4 0 true 5) (make-wpos 0 0 false 5)
                               (make-wpos 0 1 false 5) (make-wpos 0 2 false 5)
                               (make-wpos 0 3 false 5) (make-wpos 0 4 false 5))
                         (second puzz03)))
(check-expect
 (initial-state puzz04) (make-state
                         (map string->list (list "#######" "#.#.#.#" "#######"
                                                 "#.#.#.#" "#######" "#.#.#.#"
                                                 "#######"))
                         (list (make-wpos 0 0 true 7) (make-wpos 2 0 true 7)
                               (make-wpos 4 0 true 7) (make-wpos 6 0 true 7)
                               (make-wpos 0 0 false 7) (make-wpos 0 2 false 7)
                               (make-wpos 0 4 false 7) (make-wpos 0 6 false 7))
                         (second puzz04)))


;; (extract-wpos g wp) produces the list of characters corresponding the to
;; wp within g. the extracted list of characters may be a combination of #'s
;; and letters. 
;; extract-wpos: Grid WPos -> (listof Char)
;; requires: the row and col of the wp has to exist in g and the length
;; of the wp cannot exceed the boundaries of the g from which the wp is placed.
;; Examples:
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local [;; local constant for the wpos-col value of wp 
          (define colcount (wpos-col wp))
          ;; (torow g count wp) goes to the row of the given wp, and calls
          ;; onto the local function: torow to extract the characters at wp
          ;; torow: Grid Nat WPos -> (listof Char)
          (define (torow g count wp)
            (cond [(zero? count) (tocol (first g) 0 wp)]
                  [else (torow (rest g) (sub1 count) wp)]))
          ;; (tocol line count wp) produces the list of characters of the
          ;; given line up to the length of wp once the count is equal to
          ;; the wpos-col of wp. 
          (define (tocol line count wp)
            (cond  [(= count (wpos-col wp)) 
                    (foldl (lambda (f rror)
                             (cond [(= (length rror) (wpos-len wp)) rror]
                                   [else (append rror (list f))]))
                           empty line)]
                   [else (tocol (rest line) (add1 count) wp)]))]
    (cond [(wpos-horiz? wp) (torow g (wpos-row wp) wp)]
          [else (torow (transpose g) (wpos-row (flip wp)) (flip wp))])))

;; Tests:
(check-expect (extract-wpos (map string->list (first puzz02))
                            (make-wpos 1 6 true 4)) '(#\# #\# #\# #\#))
(check-expect (extract-wpos (map string->list (first puzz03))
                            (make-wpos 0 0 false 2)) '(#\# #\#))


;; (replace-wpos g wp loc) produces the grid with the wp replaced by the loc.
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: value of wpos-len wp = length loc 
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local [;; (removefn loc len) removes len number of elements from the front
          ;; of the list.
          ;; removefn: (listof Char) Nat -> (listof Char)
          (define (removefn loc len)
            (cond [(zero? len) loc]
                  [else (removefn (rest loc) (sub1 len))]))
          ;; (replaced line col) produces a list of characters with the loc
          ;; embedded at the desired col value.
          ;; replace: (listof Char) Nat -> (listof Char) 
          (define (replaced line col)
            (cond [(zero? col) (append loc (removefn line (length loc)))]
                  [else (cons (first line) (replaced (rest line) (sub1 col)))]))
          ;; (make-grid g count wp) produces the grid with the embedded loc
          ;; at the given wp.
          ;; make-grid: Grid Nat WPos -> Grid 
          (define (make-grid g count wp)
            (cond [(= count (wpos-row wp))
                   (cons (replaced (first g) (wpos-col wp)) (rest g))]
                  [else (cons (first g)
                              (make-grid (rest g) (add1 count) wp))]))]
    (cond [(wpos-horiz? wp) (make-grid g 0 wp)]
          [else (transpose (make-grid (transpose g) 0 (flip wp)))])))

;; Tests:
(check-expect (replace-wpos '((#\A #\B #\C) (#\D #\E #\F) (#\H #\I #\J))
                            (make-wpos 1 1 false 2) '(#\Y #\O))
              '((#\A #\B #\C) (#\D #\Y #\F) (#\H #\O #\J)))
(check-expect (replace-wpos '((#\# #\# #\#)) (make-wpos 0 0 true 3)
                            '(#\C #\A #\T))
              '((#\C #\A #\T)))


;; (fit? word cells) determines if the word can successfully be placed in the
;; corresponding cells. 
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K"))false)

(define (fit? word cells)
  (local [;; (match? word cells) determines if the word can be placed in cells
          ;; match?: (listof Char) (listof Char) -> Bool 
          (define (match? word cells)
            (cond [(empty? word) true]
                  [(or (char=? (first word) (first cells))
                       (char=? (first cells) #\#))
                   (match? (rest word) (rest cells))]
                  [else false]))]
    (cond [(equal? word cells) true]
          [(not (= (length word) (length cells))) false]
          [else (match? word cells)])))

;; Tests:
(check-expect (fit? (string->list "STARWARS") (string->list "STARWARS")) true)
(check-expect (fit? (string->list "YOUTUBE") (string->list "R##TUBE")) false)
(check-expect (fit? (string->list "FIREMIXTAPE") (string->list "N#PE")) false)
(check-expect (fit? empty empty) true)


;; (neighbours s) produces a list of State that represents valid neighbour
;; States with one additional word placed in the puzzle.
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s)
  (local [;; -------------------------------------------------------------------
          ;; local constant for the loloc when mapping string->list to the words
          ;; of the given s
          (define loloc (map string->list (state-words s)))
          ;; (count loc) produces the number of non # characters in loc
          ;; count: (listof Char) -> Nat 
          (define (count loc)
            (cond [(empty? loc) 0]
                  [(char=? (first loc) #\#) (count (rest loc))]
                  [else (add1 (count (rest loc)))]))
          ;; (r loa visited) removes every element of visited from loa.
          ;; r: (listof Any) (listof Any) -> (listof Any) 
          (define (r loloc visited)
            (cond [(empty? visited) loloc]
                  [else (r (remove (first visited) loloc) (rest visited))]))
          ;; -------------------------------------------------------------------
          ;; (best-wpos g lowpos max-so-far bwpos) produces the bwpos to utilize
          ;; for our neighbours function, the best WPos is the WPos in which has
          ;; the most filled in characters. Done by using accumulator values. 
          ;; best-wpos: Grid (listof WPos) Nat WPos -> WPos 
          (define (best-wpos g lowpos max-so-far bwpos)
            (cond [(empty? lowpos) bwpos]
                  [(> (count (extract-wpos g (first lowpos))) max-so-far)
                   (best-wpos g (rest lowpos)
                              (count (extract-wpos g (first lowpos)))
                              (first lowpos))]
                  [else (best-wpos g (rest lowpos) max-so-far bwpos)]))
          ;; local constant for best WPos i.e. most filled in characters there. 
          (define bwpos (best-wpos (state-grid s) (state-positions s)
                                   0 (first (state-positions s))))
          ;; -------------------------------------------------------------------
          ;; (bword loloc) produces a word (represented by a list of characters)
          ;; when it is a fit with the extracted WPos. if the loloc is exhausted
          ;; false is produced.
          ;; bword: (listof (listof Char)) -> (anyof Bool (listof Char))
          (define (bword loloc)
            (cond [(empty? loloc) false]
                  [(fit? (first loloc)
                         (extract-wpos (state-grid s) bwpos)) (first loloc)]
                  [else (bword (rest loloc))]))
          ;; -------------------------------------------------------------------
          ;; (create-list lostr visited) produces a list of State that
          ;; represents valid neighbour States with one additional word placed
          ;; in the puzzle. Done with  usage of top level function: replace-wpos
          ;; and local functions: r and bword. the parameter lostr is a
          ;; ride-along parameter. visited keeps track of words used in previous
          ;; states in order to produce unique grids, also making the
          ;; list of words per state correct. A word is a list of characters.
          ;; create-list: (listof Str) (listof Char) -> (listof State)
          (define (create-list lostr visited)
            (cond [(false? (bword (r loloc visited))) empty]
                  [else (cons (make-state
                               (replace-wpos (state-grid s) bwpos
                                             (bword (r loloc visited)))
                               (r (state-positions s) (list bwpos))
                               (r lostr (list (list->string
                                             (bword (r loloc visited))))))
                              (create-list
                               lostr (cons (bword (r loloc visited))
                                         visited)))]))]
    ;; -------------------------------------------------------------------------
    (create-list (state-words s) empty)))

;; Tests:
(check-expect (neighbours my-state)
              (list (make-state (list (list #\C #\A #\T)
                                      (list #\# #\. #\.)
                                      (list #\# #\. #\.))
                                (list (make-wpos 0 0 false 3)) (list "CAR"))
                    (make-state (list (list #\C #\A #\R)
                                      (list #\# #\. #\.)
                                      (list #\# #\. #\.))
                                (list (make-wpos 0 0 false 3)) (list "CAT"))))
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))
;; Examples:
(check-expect (criss-cross (list (list "###") (list "CATS")))
              false)
(check-expect (criss-cross puzz02) '("........K.."
                                     "......ADAM." "........R.."
                                     ".....ALBERT" "......E.N.."
                                     "......S...." "......L...."
                                     "...DAVE...." "...A..Y...."
                                     "JOHN......."))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

;; Tests:
(check-expect (criss-cross (list (list "####") (list "FIRE"))) (list "FIRE"))
(check-expect (criss-cross puzz01) '("CAT"))


;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:
#| Runtime: 12.2s IRL 
(disp (criss-cross (read-puzzle "puzzle01.txt")))
(disp (criss-cross (read-puzzle "puzzle02.txt")))
(disp (criss-cross (read-puzzle "puzzle03.txt")))
(disp (criss-cross (read-puzzle "puzzle04.txt")))
(disp (criss-cross (read-puzzle "puzzle05.txt")))
(disp (criss-cross (read-puzzle "puzzle06.txt")))
(disp (criss-cross (read-puzzle "puzzle07.txt")))
(disp (criss-cross (read-puzzle "puzzle08.txt")))
(disp (criss-cross (read-puzzle "puzzle09.txt")))
(disp (criss-cross (read-puzzle "puzzle10.txt")))
|#
;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window