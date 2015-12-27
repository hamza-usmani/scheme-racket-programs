;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wonderdiet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;(nutri-info serving-size fat carb prot)   
;;   consumes 4 numbers as a structure
;; nutri-info: Nat Nat Nat Nat -> Nat Nat Nat Nat
;; requires: serving-size, fat, carb, prot >= 0
;; Examples:

(check-expect (make-nutri-info 20 5 6 7)
              (make-nutri-info 20 5 6 7))
(check-expect (make-nutri-info 1 1 1 1)
              (make-nutri-info 1 1 1 1))
 
(define-struct nutri-info (serving-size fat carb prot))

;Test

(check-expect (make-nutri-info 30 1 1 1 )
              (make-nutri-info 30 1 1 1 ))
(check-expect (make-nutri-info 25 2 2 2)
              (make-nutri-info 25 2 2 2))


;;(valid-nutri-info? Nutri-Info) checks if Nutri-Info   
;;   conditions are true in the structure of nutri-info
;; valid-nutri-info: Nutri-Info -> Bool
;; A Nutri-Info is a (make-nutri-info Nat Nat Nat Nat)
;; serving-size, fat, carb, prot >= 0
;; Examples:

(check-expect (valid-nutri-info? (make-nutri-info 25 2 1 5)) true)
(check-expect (valid-nutri-info? (make-nutri-info 25 -2 1 5)) false)
(check-expect (valid-nutri-info? (make-nutri-info 4 2 2 1)) false)


(define (valid-nutri-info? Nutri-Info)
 (and (>= (nutri-info-serving-size Nutri-Info) 0)
      (>= (nutri-info-fat Nutri-Info) 0)
      (>= (nutri-info-carb Nutri-Info) 0)
      (>= (nutri-info-prot Nutri-Info) 0)
      (<= (+(nutri-info-fat Nutri-Info)
           (nutri-info-carb Nutri-Info)
           (nutri-info-prot Nutri-Info))
          (nutri-info-serving-size Nutri-Info))))

;;Tests

(check-expect (valid-nutri-info? (make-nutri-info 25 2 -1 5)) false)
(check-expect (valid-nutri-info? (make-nutri-info -1 1 1 1)) false)
(check-expect (valid-nutri-info? (make-nutri-info 25 2 1 9)) true)
(check-expect (valid-nutri-info? (make-nutri-info -2 -3 3 4)) false)
(check-expect (valid-nutri-info? (make-nutri-info -20 2 3 4)) false)
(check-expect (valid-nutri-info? (make-nutri-info 1 2 2 2)) false)


;; (higher-protein Nutri-Info-1 Nutri-Info-2) determines which of the
;;      Nutri-Info types has larger protein value
;; higher-protein: Nutri-Info Nutri-Info -> Nutri-Info
;; A Nutri-Info is a (make-nutri-info Nat Nat Nat Nat)
;; Examples:

(check-expect (higher-protein (make-nutri-info 25 3 2 6)
                              (make-nutri-info 21 1 1 1))
                              (make-nutri-info 25 3 2 6))

(check-expect (higher-protein (make-nutri-info 20 2 2 9)
                              (make-nutri-info 17 2 4 1))
                              (make-nutri-info 20 2 2 9))

(define (higher-protein Nutri-Info1 Nutri-Info2)
  (cond [(> (nutri-info-prot Nutri-Info2)
             (nutri-info-prot Nutri-Info1))
             Nutri-Info2] 

        [(> (nutri-info-prot Nutri-Info1)
             (nutri-info-prot Nutri-Info2))
             Nutri-Info1]

        [(= (nutri-info-prot Nutri-Info1) (nutri-info-prot Nutri-Info2))
         (cond
               [(> (nutri-info-serving-size Nutri-Info1)
                   (nutri-info-serving-size Nutri-Info2))
                Nutri-Info2]
               
               [else Nutri-Info1])]))

;Tests
(check-expect (higher-protein (make-nutri-info 1 1 1 9)
                              (make-nutri-info 1 1 1 2))
                              (make-nutri-info 1 1 1 9))

(check-expect (higher-protein (make-nutri-info 2 2 2 3)
                              (make-nutri-info 2 2 2 6))
                              (make-nutri-info 2 2 2 6))

(check-expect (higher-protein (make-nutri-info 22 4 1 5)
                              (make-nutri-info 25 6 8 9))
                              (make-nutri-info 25 6 8 9))

(check-expect (higher-protein (make-nutri-info 16 8 3 5)
                              (make-nutri-info 25 2 2 5))
                              (make-nutri-info 16 8 3 5))

;; (combine-nutri-info Nutri-Info-1 Nutri-Info-2) adds Nutri-Info 
;;      entries essentially combining them and producing combination
;; combine-nutri-info: Nutri-Info Nutri-Info -> Nutri-Info
;; Nutri-Info is a (make-nutri-info Nat Nat Nat Nat)
;; Examples:
(check-expect (combine-nutri-info (make-nutri-info 10 2 2 2)
                                  (make-nutri-info 1 1 1 1))
                                  (make-nutri-info 11 3 3 3))

(check-expect (combine-nutri-info (make-nutri-info 5 1 1 1)
                                  (make-nutri-info 2 2 2 2))
                                  (make-nutri-info 7 3 3 3))

(define (combine-nutri-info Nutri-Info1 Nutri-Info2)
(make-nutri-info 
 (+ (nutri-info-serving-size Nutri-Info1) 
    (nutri-info-serving-size Nutri-Info2))
 
 (+ (nutri-info-fat Nutri-Info1)
    (nutri-info-fat Nutri-Info2))
 
 (+ (nutri-info-carb Nutri-Info1)
    (nutri-info-carb Nutri-Info2))
 
 (+ (nutri-info-prot Nutri-Info1)
    (nutri-info-prot Nutri-Info2))))

;;Tests

(check-expect (combine-nutri-info (make-nutri-info 1 1 1 1)
                                  (make-nutri-info 1 1 1 1))
                                  (make-nutri-info 2 2 2 2))

(check-expect (combine-nutri-info (make-nutri-info 4 3 2 1)
                                  (make-nutri-info 4 4 2 1))
                                  (make-nutri-info 8 7 4 2))

;; (good-combo? Nutri-Info-1 Nutri-Info-2) determines if second
;;      Nutri-Info can be eaten in hour following the first Nutri-Info
;; good-combo?: Nutri-Info Nutri-Info -> Bool
;; Nutri-Info is a (make-nutri-info Nat Nat Nat Nat)
;; Examples:

(check-expect (good-combo? (make-nutri-info 30 7 4 3)
                           (make-nutri-info 29 6 3 2)) true)

(check-expect (good-combo? (make-nutri-info 36 20 10 2)
                           (make-nutri-info 28 2 2 2)) false)

(define (good-combo? Nutri-Info1 Nutri-Info2)
  (and (<= (abs (- (nutri-info-fat Nutri-Info1) (nutri-info-fat Nutri-Info2))) 5)
       (<= (abs (- (nutri-info-carb Nutri-Info1) (nutri-info-carb Nutri-Info2))) 5)
       (<= (abs (- (nutri-info-prot Nutri-Info1) (nutri-info-prot Nutri-Info2))) 5))) 

;;Tests

(check-expect (good-combo? (make-nutri-info 30 1 1 1)
                           (make-nutri-info 29 2 2 2)) true)

(check-expect (good-combo? (make-nutri-info 50 10 10 10)
                           (make-nutri-info 30 1 1 1)) false)
           
      
 