;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; (singletons numlst) produceslist containing numbers with zero duplicates
;;    in the consumed list
;; singletons: (listof Num) -> (listof Num)
;; Examples:
(check-expect (singletons (list 1  2 3 4 5)) (list 1 2 3 4 5))
(check-expect (singletons '(1 1 1 1 1 2)) '(2))
(check-expect (singletons '(1 2 3 3)) '(1 2))

(define (singletons numlst)
  (foldr
   (lambda (a b)
     (cond
       [(= (sub1 (length numlst))
           (length (filter (lambda (c) (not (equal? c a))) numlst)))
        (cons a b)]
       [else b])) empty numlst))

;; Tests:

(check-expect (singletons (list)) empty)
(check-expect (singletons '(9 9 9 9)) (list))
(check-expect (singletons (list 1 2 3 4 5)) (list 1 2 3 4 5))

;; (duplicates numlst) produces list of numbs with only numbers
;;    that have duplicates in consumed list
;; duplicates: (listof Num) -> (listof Num)
;; Examples:

(check-expect (duplicates '(4 4 4 4 4)) '(4))
(check-expect (duplicates '(9 8 7 6 5)) empty)

(define (duplicates numlst)
  (foldr
   (lambda (a b)
     
     (cond
       
       [(= (- (length numlst) 1 )
           (length (filter
                    (lambda (x) (not (equal? x a))) numlst))) b]
       [else
        (cons a (filter
                 (lambda (x1) (not (equal? a x1))) b))])) empty numlst))

;; Tests:
(check-expect (duplicates '(0 0 0 0 0 0 0 1 1)) '(0 1))
(check-expect (duplicates '(1111)) empty)
(check-expect (duplicates (list)) empty)