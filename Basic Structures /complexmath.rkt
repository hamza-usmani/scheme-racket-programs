;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; (posn-mult posn-1 posn-2) takes two positions and produces
;;     a position that is a product combination of both 
;; posn-mult: (make-posn Num Num) (make-posn Num Num)-> (make-posn Num Num)
;; requires: Num can be any integer 
;; Examples: 
(check-expect (posn-mult (make-posn 1 2) (make-posn 2 3)) (make-posn -4 7))
(check-expect (posn-mult (make-posn 1 1) (make-posn 1 1)) (make-posn 0 2))

(define (posn-mult posn-1 posn-2)
  (make-posn (- (* (posn-x posn-1) (posn-x posn-2)) (* (posn-y posn-1) (posn-y posn-2)))
             (+ (* (posn-x posn-1) (posn-y posn-2)) (* (posn-x posn-2) (posn-y posn-1)))))

;;Tests
(check-expect (posn-mult (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (posn-mult (make-posn 1 1) (make-posn 2 2)) (make-posn 0 4))

;; (posn-div posn-1 posn-2) takes two positions and produces
;;     a position that is a division of position 1 by position 2
;; posn-div: (make-posn Num Num) (make-posn Num Num)-> (make-posn Num Num)
;; requires: Num (or posn values) can be any integer 
;; Examples: 
(check-expect (posn-div (make-posn 1 1) (make-posn 1 1)) (make-posn 1 0))


(define (posn-div posn-1 posn-2)
  
  (make-posn (/ (+ (* (posn-x posn-1) (posn-x posn-2))
                   (* (posn-y posn-1) (posn-y posn-2)))
                (+ (sqr (posn-x posn-2)) (sqr (posn-y posn-2))))
             
             (/ (- (* (posn-y posn-1) (posn-x posn-2))
                   (* (posn-x posn-1) (posn-y posn-2)))
                (+ (sqr (posn-x posn-2)) (sqr (posn-y posn-2))))))
;;Tests
(check-expect (posn-div (make-posn 1 1) (make-posn 2 2)) (make-posn 0.5 0))
(check-expect (posn-div (make-posn 1 2) (make-posn 3 4)) (make-posn 0.44 0.08))

;; (rotate-along-circle posn-1 angle) takes a position and angle
;;     and produces a posn that is moved by the angle around the angle
;; rotate-along-circle: Num (make-posn Num Num) -> (make-posn Num Num)
;; requires: angle is a real number, angle > 0, posn values can be any integer
;; Examples:
(check-expect (rotate-along-circle 0 (make-posn 1 1)) (make-posn 1 1)) 
(check-expect (rotate-along-circle 45 (make-posn 0 0)) (make-posn 0 0)) 

(define (rotate-along-circle angle posn-3)
  (posn-mult (make-posn (cos angle) (sin angle)) posn-3))

;;Tests
(check-expect (rotate-along-circle 90 (make-posn 0 0)) (make-posn 0 0))
(check-expect (rotate-along-circle 180 (make-posn 0 0)) (make-posn 0 0))
(check-expect (rotate-along-circle 0 (make-posn 1 0)) (make-posn 1 0))
(check-within (rotate-along-circle 45 (make-posn 1 1)) (make-posn -0.325 1.376) 0.001) 
