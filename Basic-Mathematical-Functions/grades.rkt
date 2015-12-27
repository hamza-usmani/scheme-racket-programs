;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;This short program calculates Waterloo CS Marks based off of midterm, participation, exam and assignment marks

(define (mid1 x)
  (* x 0.1))

(define (mid2 y)
  (* y 0.2))

(define (participation z)
  (* z 0.05))

(define (overall a)
  (* a 0.2))

(define (cs135-grade-sofar0 x y z a)
 (+ (mid1 x) (mid2 y) (participation z) (overall a)))

(define (cs135-grade-sofar x y z a)
(*(/(cs135-grade-sofar0 x y z a) 55) 100))   

  
(define (cs135-final-exam goingin final)
  
  (*(/(- final (* goingin 0.55)) 45) 100))  