;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;(indigestible s f c p) produces serving grams
;;     that aren't fat, carbohydrate or protein 
;; indigestible: Num Num Num Num -> Num
;; Examples:
(check-expect (indigestible 100 20 20 20) 40)
;; requires: all of s, f, c, p must be positive

(define (indigestible s f c p)
  (- s (+ f c p)))
(check-expect (indigestible 100 10 10 10) 70)
(check-expect (indigestible 100 20 30 80) -30)

;;(high-protein? s f c p) determines if 
;;     protein is greater or equal to 5
;; high-protein?: Num Num Num Num -> Bool
;; Examples:
(check-expect (high-protein? 100 20 20 20) #true)
;; requires: all of s, f, c, p must be positive

(define (high-protein? s f c p)
  (>= p 5))
(check-expect (high-protein? 100 10 10 10) #true)
(check-expect (high-protein? 100 10 10 4) #false)

;;(calories s f c p) produces 
;;     total number of calories in serving
;; indigestible: Num Num Num Num -> Num
;; Examples:
(check-expect (calories 100 20 20 20) 340)
;; requires: all of s, f, c, p must be positive

(define (calories s f c p)
  (+ (* f 9) (* c 4) (* p 4)))
(check-expect (calories 100 10 10 10) 170)
(check-expect (calories 10 2 2 2) 34)

;;(low-carb? s f c p) determines if 
;;     grams of carbohydrates less than 2
;;     or carbohydrate calories less than 5% of total calories
;; low-carb?: Num Num Num Num -> Bool
;; Examples:
(check-expect (low-carb? 100 20 1 20) #true)
;; requires: all of s, f, c, p must be positive

(define (low-carb? s f c p)
  (or (< c 2) (< (* 4 c) (* 0.05 (calories s f c p)))))
(check-expect (low-carb? 100 20 1.5 20) #true)
(check-expect (low-carb? 200 20 2 20) #true)
(check-expect (low-carb? 100 20 20 20) #false)

;;(percentcaloriesf s f c p) produces 
;;     fat calories percentage relative to total calories
;; percentcaloriesf: Num Num Num Num -> Num
;; Examples:
(check-expect (percentcaloriesf 10 1 2 2) 0.36)
;; requires: all of s, f, c, p must be positive

(define (percentcaloriesf s f c p)
  (/ (* f 9) (calories s f c p)))
(check-expect (percentcaloriesf 100 10 20 20) 0.36)

;;(percentcaloriesc s f c p) produces 
;;     carbohydrate calories percentage relative to total calories
;; percentcaloriesc: Num Num Num Num -> Num
;; Examples:
(check-expect (percentcaloriesc 10 1 2 2) 0.32)
;; requires: all of s, f, c, p must be positive

(define (percentcaloriesc s f c p)
  (/(* c 4) (calories s f c p)))
(check-expect (percentcaloriesc 100 10 20 20) 0.32)

;;(percentcaloriesp s f c p) produces 
;;     protein calories percentage relative to total calories
;; percentcaloriesp: Num Num Num Num -> Num
;; Examples:
(check-expect (percentcaloriesp 10 1 2 2) 0.32)
;; requires: all of s, f, c, p must be positive

(define (percentcaloriesp s f c p)
  (/(* p 4) (calories s f c p)))
(check-expect (percentcaloriesp 100 10 20 20) 0.32)

;; (zone? s f c p) determines if a food is balanced
;;     meaning the (fat/carbohydrate/protein) percentages are
;;     (30%/40%/30%) with a 2% inclusive margin of error
;; zone?: Num Num Num Num -> Bool
;; Examples:
(check-expect (zone? 100 3.1 10 7.50) true)
;; requires: all of s, f, c, p must be positive

(define (zone? s f c p)
  (and
   (and (>= (percentcaloriesf s f c p) 0.28)
        (<= (percentcaloriesf s f c p) 0.32))
   
   (and (>= (percentcaloriesc s f c p) 0.38)
        (<= (percentcaloriesc s f c p) 0.42))
   
   (and (>= (percentcaloriesp s f c p) 0.28)
        (<= (percentcaloriesp s f c p) 0.32))
   
   (= (+(percentcaloriesf s f c p)
        (percentcaloriesc s f c p)
        (percentcaloriesp s f c p)) 1.00))) 

(check-expect (zone? 1000 31 100 75) true)
(check-expect (zone? 100 10 10 10) false)