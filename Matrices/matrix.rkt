;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; (my-list-ref lst n) produces the nth value of a list
;; my-list-ref: (listof Any) Nat -> Any
;; requires n < lst
;; Examples:
(check-expect (my-list-ref '(a 2 c 7) 3) 7)


(define (my-list-ref lst n)
  (cond 
    [(= n 0) (first lst)]
    [else (my-list-ref (rest lst) (sub1 n))]))

;;Tests
(check-expect (my-list-ref (list 1 2 3) 0) 1)
(check-expect (my-list-ref (list 1 2 3) 2) 3) 


;; (matrix-row matrix row) produces the nth row in the matrix
;; matrix-row: (listof (listof Num)) Nat -> (listof Num)
;; Examples:
(check-expect (matrix-row (list (list 1 2 3 4 5) (list 6 7 8 9 10)
                                (list 11 12 13 14 15)) 1)
              (list 6 7 8 9 10))

(check-expect (matrix-row (list (list 1 2) (list 3 4)
                                (list 5 6)) 2)
              (list 5 6))

(define (matrix-row matrix row)
  (cond [(empty? matrix) empty]
        [else (my-list-ref matrix row)]))

;;Tests
(check-expect (matrix-row (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 0) 
              (list 1 2 3))

(check-expect (matrix-row (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 1) 
              (list 4 5 6))

(check-expect (matrix-row (list) 1) empty)


;; (matrix-col matrix col) produces the nth column in a matrix
;; matrix-col: (listof (listof Num)) Nat -> (listof Num)
;; Examples:
(check-expect (matrix-col (list (list 1 2 3) (list 4 5 6)
                                (list 7 8 9)) 0) (list 1 4 7))

(check-expect (matrix-col (list (list 1 2 3 4) (list 5 6 7 8)
                                (list 9 10 11 12)) 2) (list 3 7 11))

(define (matrix-col matrix col)
  (cond
    [(empty? matrix) empty]
    [else (cons (my-list-ref (first matrix) col)
                (matrix-col (rest matrix) col))]))

;;Tests
(check-expect (matrix-col (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 1) 
              (list 2 5 8))

(check-expect (matrix-col (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 2) 
              (list 3 6 9))

(check-expect (matrix-col (list) 1) empty)


;;(get-element matrix row col) produces the element at a row and column
;;    position in a matrix
;; get-element: (listof (listof Num)) Nat Nat -> Num
;; Examples:

(check-expect (get-element (list (list 1 2 3) (list 4 5 6)
                                 (list 7 8 9)) 1 2) 6)

(check-expect (get-element (list (list 1 2) (list 3 4)
                                 (list 5 6)) 0 0) 1)

(define (get-element matrix row col)
  (cond [(empty? matrix) empty]
        [else (matrix-row (matrix-col matrix col) row)])) 

;;Tests
(check-expect (get-element (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 1 2) 6)
(check-expect (get-element (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 0 0) 1)
(check-expect (get-element (list) 0 1) empty)

;; (matrix-add matrix1 matrix2) adds two matrices together
;; matrix-add: (listof (listof Num)) (listof (listof Num)) -> (listof (listof Num))
;; Examples:
(check-expect (matrix-add (list (list 1 0) (list 0 1))
                          (list (list 2 2) (list 2 1)))
              (list (list 3 2) (list 2 2)))

(check-expect (matrix-add (list (list 0 0) (list 0 0))
                          (list (list 0 0) (list 0 0)))
              (list (list 0 0) (list 0 0)))

(define (matrix-add matrix1 matrix2)
  (cond
    [(and (empty? matrix1) (empty? matrix2)) empty]
    [else (list (list (+ (get-element matrix1 0 0)
                         (get-element matrix2 0 0))
                      (+ (get-element matrix1 0 1)
                         (get-element matrix2 0 1)))
                (list (+ (get-element matrix1 1 0)
                         (get-element matrix2 1 0))
                      (+ (get-element matrix1 1 1)
                         (get-element matrix2 1 1))))]))

;; Tests:
(check-expect (matrix-add (list (list 3 0) (list 2 1))
                          (list (list 3 2) (list 2 1)))
              (list (list 6 2) (list 4 2)))

(check-expect (matrix-add (list) (list)) empty)