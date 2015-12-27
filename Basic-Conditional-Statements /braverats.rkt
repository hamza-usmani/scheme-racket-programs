;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname braverats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; (braverats a b) produces a symbol ('A, 'B, 'X)
;;     depending on the combination of numbers "a" and "b"
;; braverats: Num Num-> Sym
;; Examples: 
(check-expect (braverats 7 7) 'X)
;; requires: 0 <= a, b <= 7

(define (braverats a b)
  (cond
    [ (or (= a b)
      (and (= a 0) (not (= b 5)))
      (and (= b 0) (not (= a 5)))) 'X]

    [ (and (= b 7) (and (> a 1) (< a 7))) 'B] 
    [ (and (= b 6) (and (> a 0) (< a 6) (not (= a 3)))) 'B]  
    [ (and (= b 5) (and (>= a 0)(< a 5))) 'B] 
    [ (and (= b 4) (or (= a 1) (= a 2))) 'B]
    [ (and (= b 3) (or (= a 4) (= a 6))) 'B]
    [ (and (= b 2) (or (= a 1) (= a 3))) 'B]
    [ (and (= b 1) (or (= a 3) (= a 7))) 'B] 

    [else 'A]))

(check-expect (braverats 0 0) 'X)
(check-expect (braverats 4 0) 'X)
(check-expect (braverats 0 3) 'X)

(check-expect (braverats 6 7) 'B)
(check-expect (braverats 5 6) 'B)
(check-expect (braverats 4 5) 'B)
(check-expect (braverats 2 4) 'B)
(check-expect (braverats 6 3) 'B)
(check-expect (braverats 3 2) 'B)
(check-expect (braverats 7 1) 'B)

(check-expect (braverats 2 1) 'A)
(check-expect (braverats 7 6) 'A)