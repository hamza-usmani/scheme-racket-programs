;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname advanced) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; A Department-List is a (listof Str)

(define-struct staff-member (id name dept))

;; A Staff-Member is a (make-staff-member Nat Str Str)
;; requires: id is unique
;; (i.e., every staff-member with the same id also has the same name)

(define-struct salary (staff-id base bonus))

;; A Salary is a (make-salary Nat Num Num)
;; requires: base, bonus ≥ 0
;; A Staff-List is a (listof Staff-Member)
;; requires: elements are sorted by increasing id
;; A Salary-List is a (listof Salary)

(define staff1 (make-staff-member 1 "John" "Engineering"))
(define staff2 (make-staff-member 2 "Adam" "R&D"))
(define staff3 (make-staff-member 3 "Albert" "Engineering"))
(define staff4 (make-staff-member 4 "Liz" "Finance"))
(define staff5 (make-staff-member 5 "Anne" "Defence"))
(define staff6 (make-staff-member 6 "Suzy" "R&D"))
(define staff7 (make-staff-member 7 "Leslie" "R&D"))
(define staff8 (make-staff-member 8 "Josh" "Engineering"))

(define salary1 (make-salary 1 50000 10))
(define salary2 (make-salary 2 74000 250))
(define salary3 (make-salary 3 85000 10))
(define salary4 (make-salary 4 60000 0))
(define salary5 (make-salary 5 93000 100))
(define salary6 (make-salary 6 68500 0))
(define salary7 (make-salary 7 250000 0))
(define salary8 (make-salary 8 120000 0))

(define staff-list (list staff1 staff2 staff3 staff4 staff5 staff6 staff7 staff8))
(define sal-list (list salary1 salary2 salary3 salary4 salary7 salary8))

(define-struct staff-node (staff left right))
;; A Staff-Node is a (make-staff-node Staff-Member Staff-BST Staff-BST)
;; requires: ids of all Staff-Member on the left subtree are smaller than the id of Staff-Member
;; ids of all Staff-Member on the right subtree are larger than the id of Staff-Member
;; A Staff-BST is one of:
;;* empty
;;* Staff-Node

;;(add-staff-bst Staff-BST Staff-Member) consumes
;;    a consumes a Staff-BST and a Staff-Member and
;;    produces a new Staff-List with the Staff-Member added
;; add-staff-bst: Staff-BST Staff-Member -> Staff-BST
;;Examples:

(check-expect (add-staff-bst
                (make-staff-node staff3
                (make-staff-node staff1 empty empty) empty) staff4)
              (make-staff-node staff3
              (make-staff-node staff1 empty empty)
              (make-staff-node staff4 empty empty)))

(check-expect (add-staff-bst
                (make-staff-node staff2 empty
                (make-staff-node staff3 empty
                (make-staff-node staff5 empty empty))) staff1)
              (make-staff-node staff2
              (make-staff-node staff1 empty empty)
              (make-staff-node staff3 empty
              (make-staff-node staff5 empty empty))))              

(define (add-staff-bst Staff-BST Staff-Member)
  (cond [(empty? Staff-BST) (make-staff-node Staff-Member empty empty)]
        
        [(< (staff-member-id Staff-Member)
            (staff-member-id (staff-node-staff Staff-BST))) 
         (make-staff-node (staff-node-staff Staff-BST)
                          (add-staff-bst (staff-node-left Staff-BST) Staff-Member)
                          (staff-node-right Staff-BST))]
        
      [(> (staff-member-id Staff-Member)
            (staff-member-id (staff-node-staff Staff-BST))) 
         (make-staff-node (staff-node-staff Staff-BST)
                          (staff-node-left Staff-BST)
                          (add-staff-bst (staff-node-right Staff-BST) Staff-Member))]))

;;Test
(check-expect (add-staff-bst (make-staff-node staff1 empty
                                 (make-staff-node staff2 empty
                                     (make-staff-node staff4 empty empty))) staff3)
              
              (make-staff-node staff1 empty
                  (make-staff-node staff2 empty
                      (make-staff-node staff4
                          (make-staff-node staff3 empty empty) empty))))

(check-expect (add-staff-bst empty staff1)
              (make-staff-node staff1 empty empty))


;;(create-staff-bst-from-list listStaff) consumes list of Staff-Member 
;;    and produces a corresponding Staff-BST
;; create-staff-bst-from-list: (listof Staff-Member) -> Staff-BST
;; where Staff-Member is previously defined
;;Examples:

(check-expect (create-staff-bst-from-list
                (list staff1 staff4 staff5 staff8))
              (make-staff-node staff8
              (make-staff-node staff5
              (make-staff-node staff4
              (make-staff-node staff1 empty empty) empty) empty) empty))

(check-expect (create-staff-bst-from-list
                (list staff5 staff3 staff2 staff1))
              (make-staff-node staff1 empty
              (make-staff-node staff2 empty
              (make-staff-node staff3 empty
              (make-staff-node staff5 empty empty)))))


(define (create-staff-bst-from-list listStaff)
  (cond [(empty? listStaff) empty]
        [else (add-staff-bst (create-staff-bst-from-list (rest listStaff))
                             (first listStaff))]))

;;Tests

(check-expect (create-staff-bst-from-list (list staff1))
              (make-staff-node staff1 empty empty))

(check-expect (create-staff-bst-from-list
                (list staff1 staff6 staff7 staff2 staff8 staff5))
              (make-staff-node staff5
              (make-staff-node staff2
              (make-staff-node staff1 empty empty)empty)
              (make-staff-node staff8
              (make-staff-node staff7
              (make-staff-node staff6 empty empty) empty) empty)))

;;(salary-retrieve Staff-Member Salary-List) consumes a
;;    Salary-List, a Staff-Member, and produces
;;    the total salary of a Staff-Member
;; salary-retrieve: Staff-Member Salary-List -> Num
;; where Num > 0
;;Examples in the who-to-fire function


(define (salary-retrieve Staff-Member Salary-List)
  (cond
    [(empty? Salary-List) 0]
    [(= (staff-member-id Staff-Member) (salary-staff-id (first Salary-List)))
     (+ (salary-base (first Salary-List)) (salary-bonus (first Salary-List)))]
    [else (salary-retrieve Staff-Member (rest Salary-List))]))


;;(who-to-fire Staff-Member Salary-List) consumes a
;;    Salary-List, a Staff-BST, and a positive number
;;    and produces a (sorted) Staff-List of Staff-Members
;;    with salary greater than number 
;; who-to-fire Salary-List: Salary-List Staff-BST Num -> Staff-List
;; where max > 0
;;Examples

(check-expect (who-to-fire sal-list
                   (make-staff-node staff2
                   (make-staff-node staff1 empty empty)
                   (make-staff-node staff4 empty
                   (make-staff-node staff6 empty empty))) 60000)
              (list staff2))

(check-expect (who-to-fire sal-list (make-staff-node staff1 empty
                                    (make-staff-node staff3 empty
                                    (make-staff-node staff4 empty empty))) 1)
              (list staff1 staff3 staff4))


(define (who-to-fire Salary-List Staff-BST max)
  (cond [(empty? Staff-BST) empty]

        [else (append
         (who-to-fire Salary-List (staff-node-left Staff-BST) max) 

         (cond [(> (salary-retrieve (staff-node-staff Staff-BST)
                                    Salary-List) max)
                (list (staff-node-staff Staff-BST))]
               
               [else empty])
         
         (who-to-fire Salary-List (staff-node-right Staff-BST) max))]))

;;Tests
(check-expect (who-to-fire (list salary1) (make-staff-node staff1 empty empty) 60000)
              empty)

(check-expect (who-to-fire sal-list
                  (make-staff-node staff2 empty
                  (make-staff-node staff3 empty
                  (make-staff-node staff4 empty
                  (make-staff-node staff5 empty empty)))) 1)
              (list staff2 staff3 staff4))

(check-expect (who-to-fire sal-list
                   (make-staff-node staff2
                   (make-staff-node staff1 empty empty)
                   (make-staff-node staff4 empty
                   (make-staff-node staff6 empty empty))) 60000)
              (list staff2))

(check-expect (who-to-fire empty (make-staff-node staff1 empty
                                 (make-staff-node staff2 empty
                                 (make-staff-node staff4 empty empty))) 1) empty)
                      
;; (next-node Staff-BST) produces the next node that succeeds the
;;    (first staff-node-staff) of Staff-BST
;; next-node: Staff-BST -> Staff-Member
;; Examples: see final function remove-from-bst

(define (next-node Staff-BST)
  (cond
    [(empty? (staff-node-left Staff-BST))
     (staff-node-staff Staff-BST)]
    [else (next-node (staff-node-left Staff-BST))]))


;; (remove-one-node Staff-BST) produces a Staff-BST with the (first
;;        staff-node-staff) of Staff-BST removed from it
;; remove-one-node: Staff-BST -> Staff-BST
;; Examples and Tests are in the final function remove-from-bst

(define (remove-one-node Staff-BST)
  (cond
    [(and (empty? (staff-node-left Staff-BST))
          (empty? (staff-node-right Staff-BST)))
     empty]
    [(empty? (staff-node-left Staff-BST))
     (staff-node-right Staff-BST)]
    [(empty? (staff-node-right Staff-BST))
     (staff-node-left Staff-BST)]
    [else (make-staff-node (next-node (staff-node-right Staff-BST))
           (staff-node-left Staff-BST)                   
           (remove-from-bst (staff-node-right Staff-BST)
            (staff-member-id (next-node
                              (staff-node-right Staff-BST)))))]))


;; (remove-from-bst Staff-BST id) removes the Staff-Node corresponding to
;;       the id from Staff-BST
;; remove-from-bst: Staff-BST Nat -> Staff-BST
;; Examples:
(check-expect (remove-from-bst
                (make-staff-node staff3
                (make-staff-node staff2 empty empty)
                (make-staff-node staff5 empty
                (make-staff-node staff6 empty empty))) 3)
              (make-staff-node staff5
                (make-staff-node staff2 empty empty)
                (make-staff-node staff6 empty empty)))

(check-expect (remove-from-bst (make-staff-node staff1 empty empty) 1) empty)


(define (remove-from-bst Staff-BST id)
  (cond
    [(empty? Staff-BST) empty]
    [(= (staff-member-id(staff-node-staff Staff-BST)) id)
     (remove-one-node Staff-BST)]
    [else (make-staff-node
           (staff-node-staff Staff-BST)
           (remove-from-bst (staff-node-left Staff-BST)
                            id)
           (remove-from-bst (staff-node-right Staff-BST)
                            id))]))

;; Tests:

(check-expect (remove-from-bst
                (make-staff-node staff3
                (make-staff-node staff2 empty empty)
                (make-staff-node staff7 empty
                (make-staff-node staff8 empty empty))) 7)
              (make-staff-node staff3
              (make-staff-node staff2 empty empty)
              (make-staff-node staff8 empty empty)))

(check-expect (remove-from-bst empty 1) empty)






                         

                
                
                             
            
                                                        
