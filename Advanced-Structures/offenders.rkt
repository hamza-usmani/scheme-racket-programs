;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname offenders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

(define-struct email (staff-id recipients word-count reply))
;; An Email is a (make-email Nat (listof Nat) Nat Reply)
;; A Reply is one of:
;;    * empty 
;;    * (listof Email)
;; requires a reply to be a child of a parent email node, so an email can
;;    only be the reply to a single other email

(define email7 (make-email 1 (list 2 3) 5 empty))
(define email6 (make-email 2 (list 1 4 5) 10 (list email7)))
(define email5 (make-email 3 (list 6 7) 15 (list email6)))
(define email4 (make-email 1 (list 2 3 4 5 6 7) 20 empty))
(define email3 (make-email 2 (list 3 5 6) 25 empty))
(define email2 (make-email 3 (list 4 7) 30 empty))
(define email1 (make-email 1 (list 2 3 4 5 6 7) 35 (list email2 email3 email4)))


;; (total-word-count Email) consumes an Email and produces
;;     the total number of words in Email including all
;;     words in the reply to the Email
;; total-word-count: Email -> Nat

;; Examples:

(check-expect (total-word-count (make-email 1 empty 0 empty)) 0)

(check-expect (total-word-count (make-email 1  (list 1 2 3) 10 (list email3))) 35)

(define (total-word-count Email)
  
  (local [ ;; (reply-words lst-replies) consumes a list of Email and 
          ;;      produces number of words in replies to an email
          ;; reply-word-count: (listof Email) -> Nat
          
          (define (reply-words listreplies)
            
            (cond [(empty? listreplies) 0]
                  
                  [else (+ (email-word-count (first listreplies))
                           (reply-words (rest listreplies)))]))]
    
    (+ (email-word-count Email)
       (reply-words (email-reply Email)))))

;;Tests

(check-expect (total-word-count email1) 110)
(check-expect (total-word-count email6) 15)
(check-expect (total-word-count email5) 25)


;; (unique-email-senders listemails)  consumes a (listof Email)
;;     and produces list of all unique email senders
;;     in the consumed (listof Email)
;; unique-email-senders: (listof Email) -> (listof Nat)
;; Examples:

(check-expect (unique-email-senders (list)) empty)

(check-expect (unique-email-senders (list email7 email1))
              (list 3 2 1))

(define (unique-email-senders listemails)
  
  (local [;; (email-senders listemails) consumes a (listof Email) and
          ;;     produces a list of all of the senders of emails
          ;; email-senders: (listof Email) -> (listof Nat)
          
          (define (email-senders listemails)
            
            (cond [(empty? listemails) empty]
                  
                  [(empty? (email-reply (first listemails)))
                   
                   (cons (email-staff-id (first listemails))
                         (email-senders (rest listemails)))]
                  
                  [else (cons (email-staff-id (first listemails))
                              (email-senders (email-reply (first listemails))))])) 
          
          ;; (make-list-unique listemails) consumes a list of natural
          ;;    numbers and removes duplicates 
          ;; make-list-unique: (listof Nat) -> (listof Nat)
          
          (define (make-list-unique listemails)
            
            (cond [(empty? listemails) empty]
                  
                  [(member? (first listemails) (rest listemails)) 
                   (make-list-unique (rest listemails))]
                  
                  [else (cons (first listemails) (make-list-unique (rest listemails)))]))] 
    
    (make-list-unique (email-senders listemails))))

;;Tests

(check-expect (unique-email-senders
               (list (make-email 3 (list 4 7) 30 empty)
                     (make-email 2 (list 3 5 6) 25 empty)))
              (list 3 2))

(check-expect (unique-email-senders (list email6))
              (list 2 1))

(check-expect (unique-email-senders (list email7 email4))
              (list 1))


;; (sent-email-summary listemails) consumes (listof Email)
;;     and produces list of unique email senders
;; sent-email-summary: (listof Email) -> (list (listof Nat))
;;Examples

(check-expect (sent-email-summary (list email2))
              (list (list 3 1)))

(check-expect (sent-email-summary empty) empty)

(define (sent-email-summary listemails)
  (local [
          ;;(email-senders listemails) consumes (listof Email)
          ;;    and produces a list of all the staff ids that send email
          ;; email-senders: (listof Email) -> (listof Nat)
          ;; requirements and tests same as parent function
          
          (define (email-senders listemails)
            
            (cond [(empty? listemails) empty]
                  
                  [(empty? (email-reply (first listemails)))
                   (cons (email-staff-id (first listemails))
                         (email-senders (rest listemails)))]
                  
                  [else (cons (email-staff-id (first listemails))
                              (email-senders (email-reply (first listemails))))]))
          
          ;;(countsent id listemails) consumes a Natural number and
          ;;    (listof Email) and produces the
          ;;    number of emails that the 
          ;;    staff member sends 
          ;; countsent: Nat (listof Email) -> Nat
          ;; requirements and tests same as parent function
          
          (define (countsent id listemails) 
            (cond [(empty? listemails) 0] 
                  [(= id (first listemails)) 
                   (+ 1 (countsent id (rest listemails)))]
                  [else (countsent id (rest listemails))]))
          
          ;;(createlist listemails counter) consumes a (listof Email)
          ;;    and a natural number and produces a list
          ;;    of a list of unique-senders
          ;;    paired with number of emails sent by that sender
          ;; createlist: (listof Email) Nat -> (listof (listof Email Nat)
          ;; requirements and tests same as parent function
          
          (define (createlist listemails counter)
            (cond [(= counter -1) empty] 
                  
                  [else (cons
                         (list
                          (list-ref (unique-email-senders listemails) counter)  
                          (countsent (list-ref (unique-email-senders listemails) counter) 
                                     (email-senders listemails)))
                         
                         (createlist listemails (sub1 counter)))]))]   
    
    (createlist listemails (- (length (unique-email-senders listemails)) 1))))

;;Test

(check-expect (sent-email-summary (list email1))
              (list (list 1 2) (list 2 1) (list 3 1)))

(check-expect (sent-email-summary (list email6 email7))
              (list (list 1 1) (list 2 1)))

(check-expect (sent-email-summary (list (make-email 1 (list 2 3) 5 empty)))
              (list (list 1 1)))


;; (email-offenders listemails threshold) consumes a (listof Email)
;;    and a Natural number and produces a sorted list of
;;    all staff id that have sent more emails than the threshold
;; email-offenders: (listof Email) Nat -> (listof Num)

;;Examples

(check-expect (email-offenders empty 0) empty)

(define (email-offenders listemails threshold)
  
  (local [
          ;;(one-offender  emailsent threshold counter counter2)
          ;;    consumes a (listof Email), a Natural number,
          ;;    a number, and a number to be used as counters
          ;;    and produces a list of all staff
          ;;    ids who have sent more emails than the threshold
          ;; one-offender: (listof Email) Nat Num Num -> (listof Num)
          ;; requirements and tests same as parent function
          
          (define (one-offender emailsent threshold counter counter2)
            (cond [(empty? emailsent) empty]
                  [(= counter -1) empty]
                  ;[(= counter2 -1) empty]
                  
                  [(= (email-staff-id (list-ref emailsent counter))
                      (first (list-ref (sent-email-summary emailsent) counter2)))
                   
                   (cond [(> (second (list-ref (sent-email-summary emailsent) counter2))
                             threshold)
                          
                          (cons  (email-staff-id (list-ref emailsent counter))
                                 (one-offender emailsent threshold (sub1 counter)
                                               (- (length (sent-email-summary emailsent)) 1)))]
                         
                         [else (one-offender emailsent threshold (sub1 counter)
                                             (- (length (sent-email-summary emailsent)) 1))])]
                  
                  [else (one-offender emailsent threshold counter (sub1 counter2))]))
          
          ;; (listone emailsent threshold counter)
          ;;    consumes a (listof Email), a Natural number,
          ;;    a number, to be used as counter and
          ;;    produces a list of all staff ids that have sent more
          ;;    emails than the threshold, except only works for when the
          ;;    list consumed has one field
          ;; listone: (listof Email) Nat Num -> (listof Num)
          ;; requirements and tests same as parent function, except
          ;; (emailsent must be a listof Email must only have one thing in it
          
          (define (listone emailsent threshold counter)
            
            (cond
              ;[(empty? emailsent) empty]
              [(= counter -1) empty]
              [(= (email-staff-id (list-ref emailsent counter))
                  (first (first (sent-email-summary emailsent))))
               
               (cond [(> (second (first (sent-email-summary emailsent)))
                         threshold)
                      
                      (cons (email-staff-id (list-ref emailsent counter))
                            (listone emailsent threshold (sub1 counter)))]
                     
                     [else (listone emailsent threshold (sub1 counter))])]))
          
          ;[else (listone emailsent threshold (sub1 counter))] 
          
          ;; (make-list-unique listemails) consumes a list of natural
          ;;    numbers and removes duplicates 
          ;; make-list-unique: (listof Nat) -> (listof Nat)
          
          (define (make-list-unique listemails)
            
            (cond [(empty? listemails) empty]
                  
                  [(member? (first listemails) (rest listemails)) 
                   (make-list-unique (rest listemails))]
                  
                  [else (cons (first listemails) (make-list-unique (rest listemails)))]))]
    
    (cond [(= (length listemails) 1)
           (sort (make-list-unique
                  (listone listemails threshold (- (length listemails) 1))) <)]
          
          [else (sort (make-list-unique
                       (one-offender listemails threshold (- (length listemails) 1)
                                     (- (length (sent-email-summary listemails)) 1))) <)]))) 

;;Tests

(check-expect (email-offenders (list email1) 3) empty)

(check-expect (email-offenders (list email1) 1) (list 1))

(check-expect (email-offenders (list email7 email4) 0) (list 1))

(check-expect (email-offenders (list email1 email2 email3 email4 email5) 0)
              (list 1 2 3))

(check-expect (email-offenders (list email1 email2 email3 email4 email5) 2)
              empty)














