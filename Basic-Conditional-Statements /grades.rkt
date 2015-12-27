;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;(mid1 midterm1)
;;     multiplies midterm 1 mark by 0.1 to give weighted 10% mark
;;midterm1: Num -> Num
;;Examples:
(check-expect (mid1 10) 1)
;;requires: 0<= m1 <= 100

(define (mid1 midterm1)
  (* midterm1 0.1))
(check-expect (mid1 1) 0.1)
(check-expect (mid1 100) 10)

;;(mid2 midterm2)
;;     multiplies midterm 1 mark by 0.2 to give weighted 20% mark
;;midterm2: Num -> Num
;;Examples:
(check-expect (mid2 10) 2)
;;requires: 0<= m2 <= 100

(define (mid2 midterm2)
  (* midterm2 0.2))
(check-expect (mid2 1) 0.2)
(check-expect (mid2 100) 20)

;; (participation partmark)
;;     multiplies participation mark by 0.05 to give weighted 5% mark
;; participation: Num -> Num
;; Examples:
(check-expect (participation 10) 0.5)
;;requires 0 <= partmark <= 100

(define (participation partmark)
  (* partmark 0.05))
(check-expect (participation 1) 0.05)
(check-expect (participation 100) 5) 

;;(assignments assmarks)
;;    multiplies assignments mark by 0.2 to givve weighted 20% mark
;;assignments: Num-> Num
;;Examples:
(check-expect (assignments 10) 2)
;;requires 0 <= assmarks <= 100

(define (assignments assmarks)
  (* assmarks 0.2))
(check-expect (assignments 1) 0.2)
(check-expect (assignments 100) 20)

;;(exam exammark)
;;    multiplies exam mark by 0.45 to givve weighted 45% mark
;;exam: Num-> Num
;;Examples:
(check-expect (exam 10) 4.5)
;;requires 0 <= exammark <= 100

(define (exam exammark)
  (* exammark 0.45))
(check-expect (exam 1) 0.45)
(check-expect (exam 100) 45)

;;(finalgrade midterm1 midterm2 partmark assmarks exammark)
;;    produces sum of weighted midterms, participation, assignments, and exam marks
;;finalgrade: Num Num Num Num Num -> Num
;;Examples:
(check-expect (finalgrade 10 10 10 10 10) 10)
;;requires 0 <= midterm1, midterm2, partmark, assmarks, exammark <= 100

(define (finalgrade midterm1 midterm2 partmark assmarks exammark)
  (+ (mid1 midterm1) (mid2 midterm2) (participation partmark)
     (assignments assmarks) (exam exammark)))

(check-expect (finalgrade 100 100 100 100 100)100)
(check-expect (finalgrade 1 1 1 1 100) 45.55)

;;(cs135-final-grade midterm1 midterm2 partmark assmarks exammark)
;;    produces final grade in course, either 46  or weighted grade (lower value)
;;cs135-final-grade: Num Num Num Num Num -> Num
;;Examples:
(check-expect (cs135-final-grade 10 10 10 10 10) 10)
;;requires 0 <= midterm1, midterm2, partmark, assmarks, exammark <= 100

(define (cs135-final-grade midterm1 midterm2 partmark assmarks exammark)
  (cond
    [ (or (< assmarks 50) (< exammark 50))
      (cond
        [ (< (finalgrade midterm1 midterm2 partmark assmarks exammark) 46)
          (finalgrade midterm1 midterm2 partmark assmarks exammark)]
        [else 46])]
    
    [else (finalgrade midterm1 midterm2 partmark assmarks exammark)]))

(check-expect (cs135-final-grade 100 100 100 100 100) 100)
(check-expect (cs135-final-grade 100 100 100 45 100) 46)
(check-expect (cs135-final-grade 100 100 100 0 0) 35)

