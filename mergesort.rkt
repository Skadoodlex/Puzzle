;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;*********************************************
;;  Duc Le (20608114)
;;  CS 135 Fall 2015
;;  Assignment 10 Question 01
;;*********************************************
;;


;; (mergesort lst comparatorfn) produces the sorted list based on comparatorfn
;; using the implementation of the mergesort algorithm.
;; mergesort: (listof Any) (X X -> Bool) -> (listof Any)
;; Examples:
(check-expect (mergesort '(8 3 7 1 2 5 4) <) (quicksort '(8 3 7 1 2 5 4) <))
(check-expect (mergesort '(1 2 3 4 5) <) '(1 2 3 4 5))

(define (mergesort lst comparatorfn)
  (local [;; local constant for the sorted listed with the comparatorfn
          ;; using quicksort
          (define sorted (quicksort lst comparatorfn))
          ;; (take lst n) produces a list containing only the first n
          ;; elements of the list or the the entire list if it
          ;; contains fewer than n elements.
          ;; take: (listof Any) Nat -> (listof Any)
          (define (take lst n)
            (foldl (lambda (f rror) (cond [(= (length rror) n) rror]
                                          [else (append rror (list f))]))
                   empty lst))
          ;; taken from module 6
          ;; (merge lon1 lon2) produces the merged list based on comparatorfn
          ;; merge: (listof Num) (listof Num) -> (listof Num) 
          (define (merge lon1 lon2)
            (cond [(and (empty? lon1) (cons? lon2)) lon2]
                  [(and (cons? lon1) (empty? lon2)) lon1]
                  [(and (cons? lon1) (cons? lon2))
                   (cond [(comparatorfn (first lon1) (first lon2))
                          (cons (first lon1) (merge (rest lon1) lon2))]
                         [else (cons (first lon2) (merge lon1 (rest lon2)))])]
                  ))]
    (cond [(equal? lst sorted) lst]
          [else (merge
                 (mergesort (take lst (ceiling (/ (length lst) 2)))
                            comparatorfn)
                 (mergesort (take (reverse lst) (floor (/ (length lst) 2)))
                            comparatorfn))])))

;; Tests:
(check-expect (mergesort (build-list 100 identity) <)
              (build-list 100 identity))
(check-expect (mergesort '(2 3 4 1 10 20 54 1 2 4 5 7 6 2 401 3 1 1 5 7 10)
                         <=)
              (quicksort
               '(2 3 4 1 10 20 54 1 2 4 5 7 6 2 401 3 1 1 5 7 10) <=))
(check-expect (mergesort '(#\z #\x #\y #\w #\l #\m #\n #\o #\P #\q #\r #\s #\t)
                         char<=?)
              (quicksort '(#\z #\x #\y #\w #\l #\m #\n #\o #\P #\q #\r #\s #\t)
                         char<=?))
(check-expect (mergesort '("puzzles" "are" "awesome") string<=?)
              (quicksort '("puzzles" "are" "awesome") string<=?))