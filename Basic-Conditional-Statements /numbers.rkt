;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************


;;(sum-list consumes list of numbers and   
;;   produces the sum of the list
;; sum-list: (listof Num) -> Num 
;; requires: no requirements
;; Examples:

(check-expect (sum-list (cons 1 (cons 2 (cons 3 empty)))) 6)

(define (sum-list lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst)
          (sum-list (rest lst)))]))

;;Tests
(check-expect (sum-list (cons 0 (cons 0 (cons 0 empty)))) 0)
(check-expect (sum-list (cons 1 (cons -1 (cons 1 empty)))) 1)


;;(divide-list consumes list of numbers and number not zero  
;;   produces a list of each list value divided
;;   by the non-zero number
;; divide-list: (listof Num) Num -> (listof Num)
;; requires: Num must be non-zero
;; Examples:

(check-expect (divide-list (cons 1(cons 2(cons 3 empty))) 3)
              (cons 1/3 (cons 2/3 (cons 1 empty))))

(define (divide-list lst num)
  (cond
    [(empty? lst) empty]
    [else (cons (/ (first lst) num)
                (divide-list (rest lst) num))]))
 
;; Tests

(check-expect (divide-list (cons 0(cons 1(cons 2 empty))) 1)
              (cons 0 (cons 1 (cons 2 empty))))

(check-expect (divide-list (cons 0 (cons 1 (cons 2 (cons 3 empty)))) 1) 
              (cons 0 (cons 1 (cons 2 (cons 3 empty)))))

(check-expect (divide-list empty 1) empty)

;;(normalize-list consumes list of postiive numbers 
;;   produces a list of numbers in which each list value 
;;   is divided by the sum of the list
;; normalize-list: (listof Nat) -> (listof Nat)
;; requires: (listof Nat) cannot have sum of 0,
;;            and list must be positive numbers
;; Examples:

(check-expect (normalize-list (cons 1 (cons 2 (cons 3 empty))))
              (cons 1/6 (cons 1/3 (cons 1/2 empty))))

(check-expect (normalize-list (cons 1 (cons 1 (cons 1 (cons 1 empty)))))
              (cons 1/4 (cons 1/4 (cons 1/4 (cons 1/4 empty))))) 

(define (normalize-list lst)
  (divide-list lst (sum-list lst)))

;;Tests
(check-expect (normalize-list (cons 0 (cons 0 (cons 1 empty))))
              (cons 0 (cons 0 (cons 1 empty))))

(check-expect (normalize-list (cons 10 (cons 5 (cons 4 (cons 1 empty)))))
              (cons 1/2 (cons 1/4 (cons 1/5 (cons 1/20 empty))))) 


;;(list-replace consumes list of numbers,
;;   target and new number
;;   produces a list of numbers in which each occurence 
;;   of target is replaced by new number
;; list-replace: (listof Num) Num Num -> (listof Num)
;; requires: none
;; Examples:

(check-expect (list-replace (cons 1 (cons 2 (cons 2 empty))) 2 3)
(cons 1 (cons 3 (cons 3 empty))))

(check-expect (list-replace (cons 0 (cons 0 (cons 2 empty))) 0 8)
(cons 8 (cons 8 (cons 2 empty))))

(define (list-replace lst target new)
  (cond
    [(empty? lst) empty] 

    [ (= (first lst) target)
          (cons new (list-replace (rest lst) target new))] 

    [else (cons (first lst) (list-replace (rest lst) target new))]))

;;Tests

(check-expect (list-replace (cons 4 (cons 4 (cons 4 empty))) 4 1)
(cons 1 (cons 1 (cons 1 empty))))

(check-expect (list-replace (cons 10 (cons 20 (cons 10 (cons 40 empty)))) 10 0)
(cons 0 (cons 20 (cons 0 (cons 40 empty)))))

;;(count-repeats consumes list of integers,
;;   produces the number of adjacent duplicates
;; count-repeats: (listof Int) -> Nat
;; requires: lst must be a listof Integers
;; Examples:

(check-expect (count-repeats (cons 1 (cons 1 (cons 2 empty)))) 1)
(check-expect (count-repeats (cons 1 (cons 1 (cons 1 empty)))) 2)


(define (count-repeats lst)

  (cond
    [(empty? (first lst)) 0]
    [(empty? (rest lst)) 0]  
    
    [ (= (first lst) (first (rest lst)))
      (+ 1 (count-repeats (rest lst)))]

    [else (count-repeats (rest lst)) ]))  

;;Tests

(check-expect (count-repeats (cons 1 (cons 0 (cons 2 empty)))) 0)
(check-expect (count-repeats (cons 1 (cons 2 (cons 2 empty)))) 1)
(check-expect (count-repeats (cons empty empty)) 0) 




   
