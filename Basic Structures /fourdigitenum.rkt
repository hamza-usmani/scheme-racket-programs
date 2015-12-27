;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fourdigitenum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;(four-digit-nat e1 e2 e3 e4) consumes four digits
;;     and stores them in structure 
;; four-digit-nat: Int Int Int Int -> Int Int Int Int
;; A Four-Digit-Nat is a (make-four-digit-nat Int Int Int Int)
;; e1, e2, e3, e4 >= 0
;; Examples:
(check-expect (make-four-digit-nat 4 3 2 1)
              (make-four-digit-nat 4 3 2 1))

(check-expect (make-four-digit-nat 5 6 7 8)
              (make-four-digit-nat 5 6 7 8))

(define-struct four-digit-nat (e1 e2 e3 e4))

;;Tests

(check-expect (make-four-digit-nat 1 3 5 7)
              (make-four-digit-nat 1 3 5 7))

(check-expect (make-four-digit-nat 2 4 6 8)
              (make-four-digit-nat 2 4 6 8))


;;(next-num Four-Digit-Nat) adds exactly 1 to
;;      a 4 digit PIN (number) and produces the new number 
;; next-num: Int Int Int Int -> Int Int Int Int
;; A Four-Digit-Nat is a (make-four-digit-nat Int Int Int Int)
;; e1, e2, e3, e4 >= 0
;; Examples:

(check-expect (next-num (make-four-digit-nat 0 0 0 0))
                        (make-four-digit-nat 0 0 0 1))

(check-expect (next-num (make-four-digit-nat 0 2 1 5))
                        (make-four-digit-nat 0 2 1 6))

(define (next-num Four-Digit-Nat)
(cond [ (not (= (four-digit-nat-e4 Four-Digit-Nat) 9))
        (make-four-digit-nat
        (four-digit-nat-e1 Four-Digit-Nat)
        (four-digit-nat-e2 Four-Digit-Nat)
        (four-digit-nat-e3 Four-Digit-Nat)
        (+ (four-digit-nat-e4 Four-Digit-Nat) 1))]
      
        [ (= (four-digit-nat-e4 Four-Digit-Nat) 9) 
          
          (cond [(not (= (four-digit-nat-e3 Four-Digit-Nat) 9))
                 (make-four-digit-nat
                 (four-digit-nat-e1 Four-Digit-Nat)
                 (four-digit-nat-e2 Four-Digit-Nat)
                 (+ (four-digit-nat-e3 Four-Digit-Nat) 1) 0)]
                
                [ (= (four-digit-nat-e3 Four-Digit-Nat) 9) 
                  
                  (cond [(not (= (four-digit-nat-e2 Four-Digit-Nat) 9))
                         (make-four-digit-nat
                         (four-digit-nat-e1 Four-Digit-Nat)
                         (+ (four-digit-nat-e2 Four-Digit-Nat) 1) 0 0)]
                        
                        [ (= (four-digit-nat-e2 Four-Digit-Nat) 9) 
                          
                          (cond [(not (= (four-digit-nat-e1 Four-Digit-Nat) 9))
                                 (make-four-digit-nat
                                 (+ (four-digit-nat-e1 Four-Digit-Nat) 1) 0 0 0)]
                                
                                [ (= (four-digit-nat-e1 Four-Digit-Nat) 9)
                                  (make-four-digit-nat 0 0 0 0) ]) ]) ]) ]) )

;;Tests

(check-expect (next-num (make-four-digit-nat 9 9 9 9))
                        (make-four-digit-nat 0 0 0 0))

(check-expect (next-num (make-four-digit-nat 1 1 0 0))
                        (make-four-digit-nat 1 1 0 1))

(check-expect (next-num (make-four-digit-nat 8 9 9 9))
                        (make-four-digit-nat 9 0 0 0))

(check-expect (next-num (make-four-digit-nat 7 7 9 9))
                        (make-four-digit-nat 7 8 0 0))

(check-expect (next-num (make-four-digit-nat 1 2 3 9))
                        (make-four-digit-nat 1 2 4 0))
  