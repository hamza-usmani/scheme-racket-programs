;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rockets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; (stage fuel-mass dry-mass thrust isp) consumes 4 numbers as a structure
;; stage: Num Num Num Num -> Num Num Num Num
;; A Stage is a (make-stage Num Num Num Num)
;; Examples:

(check-expect (make-stage 1 2 3 4)
              (make-stage 1 2 3 4))

(check-expect (make-stage 5 6 7 8)
              (make-stage 5 6 7 8))

(define-struct stage (fuel-mass dry-mass thrust isp))

;;Tests

(check-expect (make-stage 0 0 0 0)
              (make-stage 0 0 0 0))

(check-expect (make-stage 9 9 8 8)
              (make-stage 9 9 8 8))

;; (rocket Stage) is function containing a list of the
;;     structure type Stage
;; A Stage is (make-stage Num Num Num Num)
;; rocket: Stage -> (listof Stage)
;; Examples:

(check-expect (rocket (make-stage  1 2 3 4))
              (cons (make-stage 1 2 3 4) empty))

(check-expect (rocket (make-stage  9 8 7 6))
              (cons (make-stage 9 8 7 6) empty))

(define (rocket Stage)
  (cons Stage empty))

;;Tests

(check-expect (rocket (make-stage  0 0 0 0))
              (cons (make-stage 0 0 0 0) empty))

(check-expect (rocket (make-stage  1 1 2 2))
              (cons (make-stage 1 1 2 2) empty))


;; (rocket-mass Rocket) produces mass of the rocket,
;;     by calculating sum of fuel and dry mass
;; rocket-mass: (Rocket -> Num)
;; A Rocket is list containing the structure Stage which is
;;     (make-stage Num Num Num Num)
;; Examples:

(check-expect (rocket-mass (cons (make-stage 1 1 1 1 )
                                 (cons (make-stage 1 1 1 1) empty))) 4)

(check-expect (rocket-mass (cons (make-stage 5 20 10 10)
                                 (cons (make-stage 10 30 5 5) empty))) 65) 

(define (rocket-mass Rocket)
  (cond [(empty? Rocket) 0]  
        [else (+ (stage-fuel-mass (first Rocket))
                 (stage-dry-mass (first Rocket))
                 (rocket-mass (rest Rocket)))]))

;;Tests

(check-expect (rocket-mass (cons (make-stage 15 20 159 8520)
                                 (cons (make-stage 20 22 222 4555) empty))) 77)

(check-expect (rocket-mass (cons (make-stage 60 60 60 70)
                                 (cons (make-stage 40 10 20 30) empty))) 170)

;; (g 9.8) is gravitational constant, expressed in m/s^2
(define G 9.8)

;; (rocket-twr Rocket) produces thrust-to-weight ratio of a Rocket
;; rocket-twr: (listof Num) -> Num
;; A Rocket is a list containing structure Stage which is
;;    (make-stage Num Num Num Num)
;; Examples:

(check-expect (rocket-twr (cons (make-stage 90 30 1629.25 3333)
                                (cons (make-stage 9 4 191.1 4500) empty)))
              (cons 1.25 (cons 1.5 empty)))

(check-expect (rocket-twr (cons  (make-stage 180 60 3258.5 6666)
                                 (cons  (make-stage 18 8 382.2 9000) empty)))
              (cons 1.25 (cons 1.5 empty)))

(define (rocket-twr Rocket)
  (cond
    [(empty? Rocket) empty]
    [else (cons (/ (stage-thrust (first Rocket))
                   (* G (+ (stage-fuel-mass (first Rocket))
                           (stage-dry-mass (first Rocket))
                           (rocket-mass (rest Rocket)))))
                
                (rocket-twr (rest Rocket)))]))
;;Tests

(check-expect (rocket-twr (cons  (make-stage 270 90 4887.75 9999)
                                 (cons  (make-stage 27 12 573.3 13500) empty)))
              (cons 1.25 (cons 1.5 empty)))

(check-expect (rocket-twr (cons  (make-stage 360 120 6517 13332)
                                 (cons  (make-stage 36 16 764.4 18000) empty)))
              (cons 1.25 (cons 1.5 empty)))

;; (rocket-delta-v Rocket) produces total velocity change of a Rocket
;; rocket-delta-v: (listof Num) -> Num
;; A Rocket is list containing the structure Stage which is
;;    (make-stage Num Num Num Num)
;; Examples:

(check-within (rocket-delta-v (cons  (make-stage 20 20 1500 e)
                                     (cons  (make-stage 10 10 150 e) empty)))
              #i2.986 0.001)

(check-within (rocket-delta-v (cons  (make-stage 60 20 1629.25 e)
                                     (cons  (make-stage 15 5 191.1 e) empty)))
              #i6.259 0.001)


(define (rocket-delta-v Rocket)
  (cond
    [(empty? Rocket) 0]
    
    [else (+ (* (stage-isp (first Rocket))
                
                (log (/ (+ (stage-fuel-mass (first Rocket))
                           (stage-dry-mass (first Rocket))
                           (rocket-mass (rest Rocket)))
                        
                        (+ (stage-dry-mass (first Rocket))
                           (rocket-mass (rest Rocket))))))
             
             (rocket-delta-v (rest Rocket)))]))

;; Tests:

(check-within (rocket-delta-v (cons  (make-stage 100 100 1000 e)
                              (cons  (make-stage 20 20 100 e) empty)))
              #i3.349 0.001)

(check-within (rocket-delta-v (cons  (make-stage 200 200 1000 e)
                              (cons  (make-stage 30 40 100 e) empty)))
              #i3.027 0.001)

