;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;;(intersection lst1 lst2) consumes two lists to produce 
;;   list of elements that are in both lists
;; intersection: (listof Any) (listof Any) -> (listof Any)
;; requirements: both lst1 and ls2 have no duplicates within them

;; Examples:

(check-expect (intersection (list) (list)) empty)
(check-expect (intersection '(10 9 8 7) '(5 6 7 8)) '(8 7))


(define (intersection lst1 lst2) 
  (filter (lambda (x) (member? x lst2)) lst1))

;;Tests
(check-expect (intersection (list 3 4 2 1) (list 1 5 7 3)) (list  3 1)) 

(check-expect (intersection (list 1 2 3 4) (list 1 2 4))
              (list 1 2 4))

(check-expect (intersection '(1 2 3) '(4 5 6)) empty)


;; (union lst1 lst2) consumes two lists to produce
;;   list of elements that exist in either list,
;;   with no duplicates
;; union: (listof Any) (listof Any) -> (listof Any)
;; Examples:

(check-expect (union '() '()) empty)

(check-expect (union '() (list 1 2)) (list 1 2))


(define (union lst1 lst2)
  (foldr (lambda (x y)
           (cond [(member? x y) y]
                 [else (cons x y)]))
         empty
         (foldr (lambda (x y) (cons x y)) lst1 lst2))) 

;;Tests

(check-expect (union (list 3 4 2 1) (list 1 5 7 3))
              (list 5 7 3 4 2 1))


(check-expect (union (list 1 1 1 2) (list 1))
              (list 1 2))

(check-expect (union (list 1 4 5 6) (list 7 8))
              (list 7 8 1 4 5 6))

;; (unique-fn lst1 eq-pred) consumes list and predicate equality
;;   and  produces the same list such that all duplicates
;;   according to equality, are removed
;; unique-fn: (listof Any) (X -> Bool) -> (listof Any)

;; Examples:

(check-expect (unique-fn '(1 5 4 5 4) =) '(1 5 4))

(check-expect (unique-fn '(1 1.05 2 1.2) (lambda (x y)
                                           (> 0.1 (abs (- x y)))))
              '(1 2 1.2))

(define (unique-fn lst1 eq-pred)
  (foldr
   (lambda (x y)
     (foldr (lambda (x1 y1)
              (cons x (filter
                       (lambda (a)
                         (not (eq-pred x a))) y)))
            empty lst1)) empty lst1))

;;Tests
(check-expect (unique-fn (list) =) empty)

(check-expect (unique-fn '(3 1 3) =) '(3 1))

;; (cross lsta lstb) consumes two lists to produce a list 
;;   of all possible pairs of elements from both lists
;; cross: (listof Any) (listof Any) -> (listof (listof Any))

;; Examples:

(check-expect (cross (list) (list)) empty)

(check-expect (cross '(1 2 3 4) '(3 2)) (list
                                         (list 4 3)
                                         (list 4 2)
                                         (list 3 3)
                                         (list 3 2)
                                         (list 2 3)
                                         (list 2 2)
                                         (list 1 3)
                                         (list 1 2)))

(check-expect (cross (list) (list 1 2 3 4)) empty)

(check-expect (cross (list 10 9) (list 8 7)) (list
                                              (list 9 8)
                                              (list 9 7)
                                              (list 10 8)
                                              (list 10 7)))

(define (cross lsta lstb)
  (foldr (lambda (x y)           
           (union (foldr
                   (lambda (x1 y)
                     (cons (list x x1) y))
                   empty lstb) y))
         empty lsta))

;; (jaccard lsta lstb) consumes two arguments of the same type
;;      and produces a number that represents how similar they are
;;      or the jaccard index of 2 non-empty lists of equal length
;; jaccard: (listof Num) (listof Num) -> Num
;; requirements that both consumed lists are
;;    non-empty and equal length lists of numbers

;; Examples:

(check-expect (jaccard (list 3 4 2 1) (list 1 5 7 3)) 1/3)

(check-expect (jaccard '(9 4 6 7 8) '(3 1 4 2 9)) 1/4)



(define (jaccard lsta lstb)
  (/ (length (unique-fn (intersection lsta lstb) =))
     (length (unique-fn (union lsta lstb)=))))

;; Tests:
(check-expect (jaccard (list 1) (list 2)) 0)

(check-expect (jaccard (list 1 2 3 4) (list 1 2 3 5)) 3/5)

(check-expect (jaccard (list 6 1 2) (list 7 8 6)) 1/5)


;; (take lst n) consumes a list and natural number n
;;    and produces a list containing only the first
;;    n elements of the list, or the entire list if it
;;    contains fewer than n elements
;; take: (listof Any) Nat -> (listof Any)
;; no requirements

;;Examples

(check-expect (take (list) 3) empty)

(check-expect (take (list 1 2 3) 1) (list 1))

(define (take lst n)
  (cond 
    [(> n (length lst)) lst]
    [(zero? n) empty]
    [else (cons (first lst)
                (take (rest lst) (sub1 n)))])) 

;;Tests

(check-expect (take (list 1 2 3) 2) (list 1 2))

(check-expect (take (list 5 6 7 8 9 10) 0) empty)

(check-expect (take (list 1 2) 3) (list 1 2))

;; A Feature Vector (FV) is a (listof Num)
;; A Document Identifier (DI) is a String
;; A Document Vector (DV) is a (list DI FV)

;; A Feature-Vector Association List (FV-AL) is
;; * empty
;; * (cons DV FV-AL)
;; require: each FV must be of the same length
;; each DV must be unique

;; A Document Pair Tuple (DPT) is a (list DI DI Num)
;; where Num corresponds to some similarity between
;; the feature vectors associated with each DI


;; (cmp-with-sim DV FV-AL similarity) Consumes a DV, a FV-AL, and similarity
;;    measure to produce  a list of DPTs, one for each DV in the FV-AL
;;    DPT produces  as follows: the first DI comes from provided DV,
;;    the second DI comes from the DV processed
;; cmp-with-sim: DV FV-AL ((list Num) (list Num) -> Num) -> (listof DPT)
;; where DV FV-AL and DPT have been previously explained
;; Examples: 

(check-expect (cmp-with-sim (list "a" (list 5 6 7))
                            (list (list "b" (list 5))
                                  (list "c" (list 6 8 7))) jaccard)
              (list (list "a" "b" 1/3) (list "a" "c" 2/4)))

(check-expect (cmp-with-sim (list "a" (list 0))
                            (list (list "a1" (list 0))) jaccard) 
              (list (list "a" "a1" 1)))



(define (cmp-with-sim DV FV-AL similarity)
  (foldr (lambda (x y) (cons (list (first DV) (first x)
                                   (similarity
                                    (second DV) (second x))) y))
         empty FV-AL))

;;Tests

(check-expect (cmp-with-sim (list "abc" (list 1 2 5))
                            (list (list "def" (list 1 3 5 7 9))
                                  (list "ghi" (list 0))) jaccard)
              (list (list "abc" "def" 1/3) (list "abc" "ghi" 0)))


(check-expect (cmp-with-sim (list "t1" (list 1 2 3 4))
                            (list (list "t1" (list 1 2 3 4))
                                  (list "t2" (list 2 5 1 6))) jaccard)
              (list (list "t1" "t1" 1) (list "t1" "t2" 1/3))) 

(check-expect  (cmp-with-sim (list "x" (list 1))
                             (list (list "x" (list 1 2 3))
                                   (list "y" (list 2 3 4))) jaccard)
               (list (list "x" "x" 1/3) (list "x" "y" 0)))


;; (find-all-exact FV-AL similarity) consumes a FV-AL and a similarity
;;    measure, to produce the list of DPTs that correspond to all
;;    ordered pairs of DIs in FV-AL that are exact duplicates of
;;    each other with their similarities as Num
;; find-all-exact: FV-AL ((list Num) (list Num) -> Num) -> (listof DPT)
;;    where DPT has been defined previously
;;Examples

(check-expect (find-all-exact (list (list "x" (list 1))
                                    (list "y" (list 1))) jaccard)
              (list (list "y" "x"  1) (list "x" "y" 1)))

(check-expect (find-all-exact (list (list "x" (list 1))
                                    (list "y" (list 2 3))) jaccard)
              (list))

(define (find-all-exact FV-AL similarity)
  (filter (lambda (x)
            (and (not (string=? (first x) (first (rest x))))
                 (= 1 (first (rest (rest x))))))
          
          (map (lambda (y)
                 (list (first (first y))
                       (first (first (rest y)))
                       (similarity (second (first y))
                                   (second (first (rest y))))))
               
               (cross FV-AL FV-AL))))         

;:Tests

(check-expect (find-all-exact (list (list "t1" (list 1 2 3 4))
                                    (list "t3" (list 4 3 2 1))) jaccard)
              (list (list "t3" "t1"  1) (list "t1" "t3" 1)))

(check-expect (find-all-exact '(("t1" (1 2 3 4))
                                ("t2" (1 3 2 4))
                                ("t3" (1 4 3 2))
                                ("t4" (0 6 7 8))) jaccard)
              (list
               (list "t3" "t1" 1)
               (list "t3" "t2" 1)
               (list "t2" "t1" 1)
               (list "t2" "t3" 1)
               (list "t1" "t2" 1)
               (list "t1" "t3" 1)))

(check-expect (find-all-exact (list (list "a" (list 1 2))
                                    (list "b" (list 2 1)))
                              jaccard)
              (list (list "b" "a" 1) (list "a" "b" 1)))


;; (redundant? DPTa DPTb) Consumes two DPTs  and determines
;;    if the tuples are redundant (i.e., represent the same set of DIs)
;; redundant?: DPT DPT -> Bool
;; Examples:
(check-expect (redundant? '("a" "b" 1) '("b" "a" 1)) true)

(check-expect (redundant? (list "x" "y" 1) (list "x" "r" 2)) false)

(define (redundant? DPTa DPTb)
  (and (or
        
        (and
         (string=? (first DPTa) (first DPTb))
         (string=? (second DPTa) (second DPTb)))
        
        (and
         (string=? (first DPTa) (second DPTb))
         (string=? (second DPTa) (first DPTb))))
       
       (= (third DPTa) (third DPTb))))

;;Tests

(check-expect (redundant? (list "B" "A" 1) (list "E" "Y" 1)) false)

(check-expect (redundant? (list "x" "y" 2) (list "x" "y" 1)) false)

;; (find-similar-within-d FV-AL d similarity) consumes an FV-AL,
;;     a threshold d ∈ [0,1], and a similarity measure
;;    and produces the DPTs of all combinations of pairs in the provided 
;;    FV-AL whose similarity is above the threshold d
;;    note that function should not return DPTs with identical
;;    DIs. In addition, it should return only tuples with non-redundant DIs
;;    Also order does not matter
;; find-similar-within-d: FV-AL Num ((list Num) (list Num)-> Num)->(listof DPT)
;; requirement: d between 0 and 1 inclusive
;; Examples

(check-expect (find-similar-within-d (list (list "a" (list 1 2))
                                           (list "b" (list 3 4))) .5 jaccard)
              empty)

(define (find-similar-within-d FV-AL d similarity)
  (unique-fn
   (foldr (lambda (x y) (foldr (lambda (a b) (cons a b)) y
                               (foldr (lambda (c e)
                                        (cond
                                          
                                          [(string=? (first x) (first c)) e]
                                          
                                          [(> (similarity (second x) (second c))
                                              d)
                                           
                                           (cons (list (first x) (first c) 
                                                       (similarity (second x)
                                                                   (second c)))
                                                 e)]  
                                          
                                          [else e]))
                                      
                                      empty FV-AL))) empty FV-AL) redundant?))


;;Tests:

(check-expect (find-similar-within-d '(("t1" (1 2 3 4))
                                       ("t2" (5 3 2 1))
                                       ("t3" (4 3 2 1)))
                                     .5 jaccard)
              '(("t1" "t2" 0.6) ("t1" "t3" 1) ("t2" "t3" 0.6)))


(check-expect (find-similar-within-d '(("a" (2 1))
                                       ("b" (1 2)))
                                     .5 jaccard)
              '(("a" "b" 1)))

