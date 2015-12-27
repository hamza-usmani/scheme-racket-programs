;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname staff) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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
(define dept-list (list "Engineering" "Defence" "R&D" "Management"))


(define (add-staff Staff-List Staff-Member)
  (cond [(empty? Staff-List) (list Staff-Member)]
        [(= (staff-member-id (first Staff-List))
            (staff-member-id Staff-Member)) Staff-List]
        [(>  (staff-member-id Staff-Member)
             (staff-member-id (first Staff-List)))
         (cons (first Staff-List) (add-staff (rest Staff-List) Staff-Member))]))


(define (update-staff-info Staff-List Staff-Member)
  (cond [(empty? Staff-List) (list Staff-Member)]
        [(= (staff-member-id (first Staff-List))
            (staff-member-id Staff-Member))
         (cons Staff-Member (rest Staff-List))]
        [(>  (staff-member-id Staff-Member)
             (staff-member-id (first Staff-List)))
         (cons (first Staff-List) (update-staff-info (rest Staff-List) Staff-Member))]))  


(define (all-staff-info Staff-List)
  (cond [(empty? Staff-List) empty]
        [else (cons (string-append
                     (number->string (staff-member-id (first Staff-List)))
                     " "
                     (staff-member-name (first Staff-List))
                     " "
                     (staff-member-dept (first Staff-List)))
                    (all-staff-info (rest Staff-List)))]))



(define (countdept Staff-List dept)
  (cond [(empty? Staff-List) 0]
        [(string=? (staff-member-dept (first Staff-List)) dept) 
         (+ 1 (countdept (rest Staff-List) dept))]
        [else (countdept (rest Staff-List) dept)]))

(define (count-staff-by-dept Staff-List Department-List)
  (cond [(empty? Department-List) empty]
        [else (cons
               (countdept Staff-List (first Department-List))
               (count-staff-by-dept Staff-List (rest Department-List)))]))


(define (salary-retrieve Staff-Member Salary-List)
  (cond
    [(empty? Salary-List) 0]
    [(= (staff-member-id Staff-Member) (salary-staff-id (first Salary-List)))
     (+ (salary-base (first Salary-List)) (salary-bonus (first Salary-List)))]
    [else (salary-retrieve Staff-Member (rest Salary-List))]))

(define (sum-dept-salary Staff-List dept Salary-List)
  (cond
    [(empty? Staff-List) 0]
    [(string=? (staff-member-dept (first Staff-List)) dept)
     (+ (salary-retrieve (first Staff-List) Salary-List)
        (sum-dept-salary (rest Staff-List) dept Salary-List))]
    [else (sum-dept-salary (rest Staff-List) dept Salary-List)]))

(define (salary-exist? Staff-Member Salary-List)
  (cond
    [(empty? Salary-List) false]
    [(= (staff-member-id Staff-Member) (salary-staff-id (first Salary-List))) false]
    [else (salary-exist? Staff-Member (rest Salary-List))]))


(define (avg-salary-by-dept Staff-List Salary-List Department-List)
  (cond
    [(empty? Department-List) empty]
    [(= 0 (first (count-staff-by-dept Staff-List Department-List)))
     (cons 0 (avg-salary-by-dept Staff-List Salary-List (rest Department-List)))]
    [(salary-exist? (first Staff-List) Salary-List)
     (avg-salary-by-dept Staff-List Salary-List (rest Department-List))]
    [else (cons (/ (sum-dept-salary Staff-List (first Department-List) Salary-List)
                   (first (count-staff-by-dept Staff-List Department-List)))
                (avg-salary-by-dept Staff-List Salary-List (rest Department-List)))]))

