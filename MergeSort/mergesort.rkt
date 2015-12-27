;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

(define (mergesort lst compare)
  (local
    [ ;; (take lst n) consumes a list and natural number n
     ;;    and produces a list containing only the first
     ;;    n elements of the list, or the entire list if it
     ;;    contains fewer than n elements
     ;; take: (listof Any) Nat -> (listof Any)
     
     (define (take lst n)
       (cond
         [(> n (length lst)) lst]
         [(zero? n) empty]
         [else (cons (first lst) (take (rest lst) (sub1 n)))]))
     
     ;; (merge lsta lstb compare) consumes 2 lists and comparator function
     ;;   to produce a merged list of the 2 consumed
     ;; merge: (listof Any) (listof Any) (X -> Bool) -> (listof Any)
     
     (define (merge lsta lstb compare)
       (cond
         [(and (empty? lsta) (cons? lstb)) lstb]
         
         [(and (cons? lsta) (empty? lstb)) lsta]
         
         [(and (cons? lsta) (cons? lstb))
          
          (cond
            
            [(compare (first lsta) (first lstb))
             (cons (first lsta)
                   (merge (rest lsta) lstb compare))]
            
            [else (cons (first lstb)
                        (merge lsta (rest lstb) compare))])]))]
    (cond
      [(empty? lst) empty]
      [(= (length lst) 1) lst]
      [else
       (merge (mergesort
               (take lst (ceiling (/ (length lst) 2))) compare)
              
              (mergesort (take (reverse lst)
                               (floor (/ (length lst) 2))) compare)
              compare)])))  