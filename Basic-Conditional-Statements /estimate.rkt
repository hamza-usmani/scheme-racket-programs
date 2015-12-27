;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname estimate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

(define random-number/big 4294967087)

;; (random-number min max) produces a random number between min and max
;; random-number: Num Num -> Num
;; Examples:
;; (random-number -1 1) -> 0.56
;; (random-number 0 0.5) -> 0.2

(define (random-number min max)
  (+ min (* (- max min) 
            (/ (random random-number/big) 
               random-number/big))))

(define i 0)
(define min-range -1)
(define max-range 1)

;; (accumulative-pi n n-points inside) approximates the value of pi
;;    using Monte Carlo approximation
;; accumulative-pi: Int Int Nat -> Num
;;   requires n and n-points to be positive integers

(define (accumulative-pi n n-points inside)
  (cond
    [(= n 0) (* 4 (/ inside n-points))]
    [(>= 1 (+ (sqr (random-number min-range max-range))
              (sqr (random-number min-range max-range))))
     (accumulative-pi (sub1 n) n-points (add1 inside))]
    [else (accumulative-pi (sub1 n) n-points inside)]))

;; (estimate-pi random-points-n) consumes a positive integer, which
;;    is the number of points, and produces the Monte Carlo approximation
;;    of pi, accomplished through the use of an accumulative recursion function
;; estimate-pi: Int -> Num
;;   requires random-points-n > 0

(define (estimate-pi random-points-n)
  (accumulative-pi random-points-n random-points-n i))