;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstract) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; (avg-without-recursion lon) consumes list of nums and
;;      produces average of the numbers, average of even numbers 
;;      and average of odd numbers
;; avg-without-recursion: (listof Num) -> (listof Num)
;; requirements: 1 even number and1 odd number are in the list at least

;; Examples:

(check-expect (avg-without-recursion (list 1 2)) (list 3/2 2 1))

(check-expect (avg-without-recursion (list 10 10 11 11)) (list 42/4 10 11))

(define (avg-without-recursion lon)
  
  (list (/ (foldr + 0 lon) (length lon))
        
        (/ (foldr + 0 (filter even? lon))
           (length (filter even? lon)))
        
        (/ (foldr + 0 (filter odd? lon)) 
           (length (filter odd? lon)))))

;;Tests

(check-expect (avg-without-recursion (list 5 2 3 5 6 7 9 25))
              (list 7.75 4 9))

(check-expect (avg-without-recursion (list 1 1 2 2))
              (list 6/4 2 1))



