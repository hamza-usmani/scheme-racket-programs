;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname prime) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

(define lst 2)

;; (factors num base) produces the number
;;    of factors based on base number 
;; factors: Nat Nat-> Nat 
;; requires num, base >= 0
;; Examples:
(check-expect (factors 4 2) 2)
(check-expect (factors 6 2) 3)

(define (factors num base)
  (cond
    [(= base num) 1]
    [(= (remainder num (add1 base))0)
     (+ 1 (factors num (add1 base)))]
    [else (factors num (add1 base))]))

;;Tests
(check-expect (factors 10 1) 4)
(check-expect (factors 1 1) 1)

;; (prime? natnum) produces a true or false
;;    if a number is prime or not, depending on
;;    the number of factors of a natural number
;; prime?: Nat -> Bool 
;; requires natnum >= 0
;; Examples:
(check-expect (prime? 3) true)
(check-expect (prime? 10) false)


(define (prime? natnum)
  (cond
    [(= (factors natnum 1) 2) true]
    [else false]))

;;Tests
(check-expect (prime? 9) false)
(check-expect (prime? 100) false)
(check-expect (prime? 31) true)

;; (next-prime nat) produces next prime number that is strictly
;;    greater than the input number 
;; next-prime: Nat -> Nat
;; requires nat> 0
;; Examples:
(check-expect (next-prime 22) 23)
(check-expect (next-prime 2) 3)

(define (next-prime nat)
  (cond
    [(prime? (add1 nat)) (add1 nat)]
    [else (next-prime (add1 nat))]))

;; Tests:
(check-expect (next-prime 19) 23)
(check-expect (next-prime 1) 2)
(check-expect (next-prime 0) 2)


;(define (prime-range nat1 nat2) produces the list
;;   of all prime numbers in the interval
;;   from nat1 to nat2
;;prime-range: Nat Nat -> (listof Nat)
;;requires nat1, nat2 >= 0
;;Examples:

(check-expect (prime-range 1 10) (list 2 3 5 7))
(check-expect (prime-range 10 1) empty)

(define (prime-range nat1 nat2)
  (cond [ (= (- nat1 nat2) 0)
          (cond [(prime? nat1) (cons nat1 empty)]
                [else empty])]
        [(> nat1 nat2) empty]
        [(> (next-prime nat1) nat2) empty]
        [ else (cond [(prime? nat1)
                      (cons nat1 (prime-range (+ nat1 1) nat2))]
                     [else (cons (next-prime nat1)
                                 (prime-range (+(next-prime nat1)1) nat2))])]))

;;Tests
(check-expect (prime-range 0 1) empty)
(check-expect (prime-range 2 2) (list 2))
(check-expect (prime-range 1 1) empty)
(check-expect (prime-range 1 5) (list 2 3 5))



