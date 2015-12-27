;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname email) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

(define-struct email-record (day-id hours-worked emails-sent))
;; An Email-Record is a (make-email-record Nat Num Nat)
;; requires: hours-worked >= 0

(define-struct daily-stats (staff-id staff-name emails))
;; A Daily-Stats is a (make-daily-stats Nat Str (listof Email-Record))
;; requires: each day-id in emails is unique

(define stats1 (list (make-email-record 1 7 20) (make-email-record 2 8 50)
                     (make-email-record 3 6 30) (make-email-record 4 8 100)
                     (make-email-record 5 7 50)))

(define em1 (make-daily-stats 1 "Justin" empty))

(define em2 (make-daily-stats 2 "Tolu" stats1))

(define stats2 (list (make-email-record 1 10 80) (make-email-record 2 5 30)
                     (make-email-record 3 7 30) (make-email-record 4 7 80)
                     (make-email-record 5 12 80)))

(define em3 (make-daily-stats 10 "Liz" stats2))


;; (avg-emails Daily-Stats) consumes a Daily-Stats
;;    and produces the average emails sent per hour
;;    by the staff member in Daily-Stats
;; avg-emails: Daily-Stats-> Num
;; Examples

(check-expect (avg-emails em1) 0)

(check-expect (avg-emails
               (make-daily-stats 1 "Bob"
                                 (list (make-email-record 1 5 50)
                                       (make-email-record 2 5 50)))) 10)

(define (avg-emails Daily-Stats)
  
  (local [;; (add-emails lst) sums total number of emails sent for 
          ;;     a specific staff member
          ;; add-emails: (listof Email-Record) -> Nat
          ;; An Email-Record is (make-email-record Nat Num Nat)
          ;; requires same requirements as the daily-stats structure
          
          (define (add-emails lst)
            (cond [(empty? lst) 0]
                  [else ( + (email-record-emails-sent (first lst))
                            (add-emails (rest lst)))]))
          
          ;; (add-hours lst) sums total number of hours 
          ;;     a staff member has worked
          ;; add-emails: (listof Email-Record) -> Num
          ;; An Email-Record is (make-email-record Nat Num Nat)
          ;; requires same requirements as the email-record structure
          
          (define (add-hours lst)
            (cond [(empty? lst) 0]
                  [else ( + (email-record-hours-worked (first lst))
                            (add-hours (rest lst)))]))] 
    
    (cond [(empty? (daily-stats-emails Daily-Stats))  0]
          
          [else (/ (add-emails (daily-stats-emails Daily-Stats))  
                   (add-hours (daily-stats-emails Daily-Stats)))]))) 

;;Tests

(check-expect (avg-emails em2) 250/36)

(check-expect (avg-emails (make-daily-stats 1 "Hamza"
                                            (list (make-email-record 1 1 10)))) 10)

(check-expect (avg-emails
               (make-daily-stats 1 "Alice"
                                 (list (make-email-record 1 1 3)
                                       (make-email-record 2 1 3)
                                       (make-email-record 2 1 3)))) 3)


;; (highest-email-record Daily-Stats) consumes a Daily-Stats
;;    and produces a (listof Email-Record) with
;;    the largest number of emails sent
;; highest-email-record: Daily-Stats-> (listof Email-Record)
;; An Email-Record is (make-email-record Nat Num Nat)
;; Examples

(check-expect (highest-email-record (make-daily-stats 1 "thisishard" empty))
              empty)

(check-expect (highest-email-record (make-daily-stats 10 "Liz"
                                                      (list (make-email-record 1 10 80)
                                                            (make-email-record 2 5 30)
                                                            (make-email-record 3 7 30)
                                                            (make-email-record 4 7 80)
                                                            (make-email-record 5 12 80)))) 
              (list (make-email-record 1 10 80)
                    (make-email-record 4 7 80)
                    (make-email-record 5 12 80)))

(define (highest-email-record Daily-Stats)
  
  (cond [(empty? (daily-stats-emails Daily-Stats)) empty]
        
        [else 
         (local [;; (max-list listemails) produces max number of emails sent 
                 ;;    in a listof emails
                 ;; find-max: (listof Email-Record) -> Nat
                 ;; same requirements as email-record structure
                 
                 (define (max-list listemails)
                   (cond [(empty? (rest listemails))(email-record-emails-sent (first listemails))]
                         
                         [(> (email-record-emails-sent (first listemails))
                             (max-list (rest listemails)))
                          
                          (email-record-emails-sent (first listemails))] 
                         
                         [else (max-list (rest listemails))]))
                 
                 ;; (max-emails max listemails) consumes a (listof Email) and
                 ;;    produces a list maximum number of
                 ;;    emails sent in an Email-Record
                 ;; max-emails: Nat (listof Email-Record) -> (listof Email-Record)
                 ;; same requirement as the email-record
                 
                 (define (max-emails max listemails)
                   (cond [(empty? listemails) empty]
                         
                         [(= (email-record-emails-sent (first listemails)) max)
                          
                          (cons (first listemails)
                                (max-emails (email-record-emails-sent (first listemails))
                                            (rest listemails)))] 
                         
                         [else (max-emails max (rest listemails))]))]
           
           
           (max-emails (max-list (daily-stats-emails Daily-Stats)) 
                       (daily-stats-emails Daily-Stats)))]))  

;;Tests

(check-expect (highest-email-record em3)
              (list (make-email-record 1 10 80)
                    (make-email-record 4 7 80)
                    (make-email-record 5 12 80)))

(check-expect (highest-email-record em2)
              (list (make-email-record 4 8 100)))