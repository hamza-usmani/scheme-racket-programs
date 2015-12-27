;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname org-chart) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

(define-struct supervisor (id subordinates))
;; A Supervisor is a (make-supervisor Nat (listof Org-Chart))
;; requires: id values are unique
;; An Org-Chart is one of:
;;* Nat
;;* Supervisor

;; (direct-reports Org-Chart id) gives a list of all subordinates in Org-Chart
;;   who fall under id
;; direct-reports: Org-Chart Nat -> (listof Nat)
;; Examples:
(check-expect (direct-reports empty 1) empty)
(check-expect (direct-reports (make-supervisor 1 empty) 1) empty)

(define (direct-reports Org-Chart id)
  (cond [(empty? Org-Chart) empty]
        [(= (supervisor-id Org-Chart)id)
         (single-level (supervisor-subordinates Org-Chart))]
        [else (search-oc (supervisor-subordinates Org-Chart) id)]))
;;Test

(check-expect (direct-reports (make-supervisor 1 (list 2)) 1)
              (list 2))

(check-expect   (direct-reports (make-supervisor 1
                  (list (make-supervisor 2
                    (list (make-supervisor 3 (list 2)))))) 2)
                (list 3 2))

(check-expect (direct-reports (make-supervisor 1 (list 2
                              (make-supervisor 3 empty))) 2) empty)

;; (single-level-node Org-Chart) produces a single-levelled list of every element in Org-Chart
;; single-level-node: (listof Org-Chart) -> (listof Nat)
;; Examples:
(check-expect (single-level-node empty) empty)
(check-expect (single-level-node (list 1)) (list 1))

(define (single-level list-of-Org-Chart)
  (cond [(empty? list-of-Org-Chart) empty]
        [(number? (first list-of-Org-Chart))
         (append (list (first list-of-Org-Chart))
                 (single-level (rest list-of-Org-Chart)))]
        [(supervisor? (first list-of-Org-Chart))
         (append (list (supervisor-id (first list-of-Org-Chart)))
                 (single-level(supervisor-subordinates (first list-of-Org-Chart)))
                 (single-level (rest list-of-Org-Chart)))]))

;; Tests
(check-expect (single-level-node (list 1 (make-supervisor 2 empty)))
              (list 1 2))
(check-expect (single-level-node (list 1 (make-supervisor
                                3 (list (make-supervisor 4 empty)))))
              (list 1 3 4))

(define (search-oc list-of-Org-Chart id)
  (cond [(empty? list-of-Org-Chart) empty]
        [else (append (direct-reports (first list-of-Org-Chart) id)
                      (search-oc (rest list-of-Org-Chart) id))]))


;; (vacation-approval Org-Chart id) returns a list of all immediate
;;   predecessors of id in Org-Chart
;; vacation-approval: Org-Chart Nat -> (listof Nat)
;; Examples:

(check-expect (vacation-approval empty 5) empty)
(check-expect (vacation-approval (make-supervisor 1 empty) 1) empty)

(define (vacation-approval Org-Chart id)
  (cond [(empty? Org-Chart) empty]
        [(number? Org-Chart) empty]
        [(= id (supervisor-id Org-Chart)) empty]
        [(member? id (direct-reports Org-Chart (supervisor-id Org-Chart)))
         (append (list (supervisor-id Org-Chart))
                 (vacation-find (supervisor-subordinates Org-Chart) id))]
        [else (vacation-find (supervisor-subordinates Org-Chart) id)]))

;; Tests
(check-expect (vacation-approval (make-supervisor 1 empty) 2) empty)


(check-expect (vacation-approval (make-supervisor 10 empty) 10) empty)

(check-expect (vacation-approval (make-supervisor 3 (list 2)) 2)
              (list 3))

(check-expect (vacation-approval (make-supervisor 1 (list 2 3 4)) 3)
              (list 1))

(check-expect (vacation-approval (make-supervisor 1 (list 2 3
                                                          (make-supervisor 5 (list 5 6)))) 6)
              (list 1 5))


;; (vacation-find list-of-Org-Chart id) searches  a list-of-Org-Chart
;;   for the id 
;; vacation-find (listof Org-Chart) Nat -> (listof Nat)
;; Examples or Tests in vacation-approval function

(define (vacation-find list-of-Org-Chart id)
  (cond [(empty? list-of-Org-Chart) empty]
        [else (append (vacation-approval (first list-of-Org-Chart) id)
                       (vacation-find (rest list-of-Org-Chart) id))]))