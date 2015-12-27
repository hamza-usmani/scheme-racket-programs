;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;These small programs calculate basic mathematical problems such
;;    as surface areas and interest on money, and height

(define (doughnut-surface-area r z)
  (* 4 (sqr pi) r z))


(define (interest r t)
  (expt (+ 1 r) t))

(define (future-value p r t)
  (* p (interest r t)))

(define g 9.8)

(define (step1 g t)
  (/ (* g (sqr t) ) 2))

(define (height v t)
  (- (* v t) (step1 g t) )) 
 


 