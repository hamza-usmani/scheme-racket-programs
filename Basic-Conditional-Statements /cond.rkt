;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;(define (p1? num) 
;;  (< num 9))

;;(define (p2? num)
;;  (= num 10))

(define (q1a num) 
  (cond [(and (p1? num) (p2? num)) 'up]
        [(and (p1? num) (not (p2? num))) 'down]
        [(and (not(p1? num)) (p2? num)) 'right]
        [else 'left])) 
 
(define (q1b num)
   (cond        
     [(p2? num) 'up]
     [else 'down]))

;;(define (p3? num)
;;  (> num 10))

(define (q1c num)
  (cond
    [(and (p1? num) (p2? num) (p3? num)) 'up]
    [(and (p1? num) (p2? num) (not (p3? num))) 'down]
    [else 'right]))   

  
