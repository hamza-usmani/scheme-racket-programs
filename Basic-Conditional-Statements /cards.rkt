;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; (card strength speed intelligence charm) consumes 4 numbers
;;     and produces a structure of them
;; card: Num Num Num Num -> Num Num Num Num
;; A Card is a (make-card Num Num Num Num)
;; no requirements
;; Examples:
(check-expect (make-card 1 2 3 4)
              (make-card 1 2 3 4))

(check-expect (make-card 2 2 3 3)
              (make-card 2 2 3 3))

(define-struct card (strength speed intelligence charm))

;;Tests

(check-expect (make-card 0 0 0 0)
              (make-card 0 0 0 0))

(check-expect (make-card 5 6 7 8)
              (make-card 5 6 7 8))


;; (card-to-list Card) consumes a Card and produces a list
;; card-to-list: Card -> (listof Num)
;; A Card is a (make-card Num Num Num Num) structure
;; no requirements
;; Examples:

(check-expect (card-to-list (make-card 1 2 3 4))
              (cons 1 (cons 2 (cons 3 (cons 4 empty)))))

(check-expect (card-to-list (make-card 5 6 7 8))
              (cons 5 (cons 6 (cons 7 (cons 8 empty)))))

(define (card-to-list Card)
  (cons (card-strength Card)
        (cons (card-speed Card)
              (cons (card-intelligence Card)
                    (cons (card-charm Card) empty)))))

;;Tests
(check-expect (card-to-list (make-card 0 0 0 0))
              (cons 0 (cons 0 (cons 0 (cons 0 empty)))))

(check-expect (card-to-list (make-card 2 2 3 3))
              (cons 2 (cons 2 (cons 3 (cons 3 empty)))))

;; (list-to-card Card) consumes a list and produces a Card
;; list-to-card: (listof Num) -> Card
;; A Card is a (make-card Num Num Num Num)
;; no requirements
;; Examples:

(check-expect (list-to-card (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              (make-card 1 2 3 4))

(check-expect (list-to-card (cons 5 (cons 6 (cons 7 (cons 8 empty)))))
              (make-card 5 6 7 8))

(define (list-to-card lst)
  (make-card
   (first lst)
   (first (rest lst))
   (first (rest (rest lst))) 
   (first (rest (rest (rest lst))))))

;;Tests

(check-expect (list-to-card (cons 0 (cons 0 (cons 0 (cons 4 empty)))))
              (make-card 0 0 0 4))

(check-expect (list-to-card (cons 2 (cons 2 (cons 3 (cons 8 empty)))))
              (make-card 2 2 3 8))


;; (card-regular? Card) checks if a Card's values sums to 10
;; card-regular: Card -> Bool
;; A Card is a (make-card Num Num Num Num) structure
;; no requirements
;; Examples:

(check-expect (card-regular? (make-card 7 1 1 1)) true)
(check-expect (card-regular? (make-card 9 9 9 9)) false) 

(define (card-regular? Card)
  (= (+ (card-strength Card)
        (card-speed Card)
        (card-intelligence Card)
        (card-charm Card)) 10))

;;Tests
(check-expect (card-regular? (make-card 5 1 3 1)) true)
(check-expect (card-regular? (make-card 8 7 6 5)) false)


;; (card-battle-temp lst1 lst2) determines if one element in a list 
;;     is greater than the matching element in the other list
;;     and produces an integer to be used
;; card-battle-temp: (listof Num) (listof Num) -> Int
;; no requirements
;; Examples:

(check-expect (card-battle-temp (cons 1 (cons 2 (cons 3 (cons 4 empty))))
                                (cons 2 (cons 3 (cons 4 (cons 5 empty))))) -4)

(check-expect (card-battle-temp (cons 4 (cons 5 (cons 6 (cons 7 empty))))
                                (cons 1 (cons 2 (cons 3 (cons 4 empty))))) +4)

(define (card-battle-temp lst1 lst2)
  
  (cond
    
    [(empty? lst1) 0]
    
    [ (> (first lst1) (first lst2))
      (+ 1 (card-battle-temp (rest lst1) (rest lst2)))] 
    
    [ (< (first lst1) (first lst2))
      (+ -1 (card-battle-temp (rest lst1) (rest lst2)))]
    
    [else (+ 0 (card-battle-temp (rest lst1) (rest lst2)))]))

;;Tests

(check-expect (card-battle-temp (cons 1 (cons 1 (cons 1 (cons 1 empty))))
                                (cons 2 (cons 3 (cons 4 (cons 5 empty))))) -4)

(check-expect (card-battle-temp (cons 9 (cons 9 (cons 9 (cons 9 empty))))
                                (cons 2 (cons 3 (cons 4 (cons 5 empty))))) +4)

(check-expect (card-battle-temp (cons 1 (cons 9 (cons 9 (cons 9 empty))))
                                (cons 2 (cons 3 (cons 4 (cons 5 empty))))) +2)

(check-expect (card-battle-temp (cons 9 (cons 1 (cons 1 (cons 1 empty))))
                                (cons 2 (cons 3 (cons 4 (cons 5 empty))))) -2)

(check-expect (card-battle-temp (cons 2 (cons 2 (cons 1 (cons 1 empty))))
                                (cons 2 (cons 2 (cons 1 (cons 1 empty))))) 0)

;; (card-battle Card1 Card2) determines if Card1
;;     wins, draws, or loses against Card2
;;     and produces symbol for win, lose or draw
;; card-battle: Card Card -> Sym
;; Examples:

(check-expect (card-battle (make-card 4 2 2 2) (make-card 7 1 1 1)) 'win)
(check-expect (card-battle (make-card 1 1 1 1) (make-card 1 1 1 1)) 'draw)

(define (card-battle Card1 Card2)
  (cond [(> (card-battle-temp (card-to-list Card1) (card-to-list Card2)) 0) 'win]
        [(< (card-battle-temp (card-to-list Card1) (card-to-list Card2)) 0) 'lose]
        [(= (card-battle-temp (card-to-list Card1) (card-to-list Card2)) 0) 'draw]))

;;Tests
(check-expect (card-battle (make-card 9 9 9 9) (make-card 1 1 1 1)) 'win)
(check-expect (card-battle (make-card 8 9 10 11) (make-card 8 9 10 11)) 'draw)
(check-expect (card-battle (make-card 1 2 1 2) (make-card 9 9 9 9)) 'lose)


;; (card-select hand Card3) determines if cards in your hand
;;     wins, draws, or loses against opponent card
;;     and produces false or true depending on answer
;; if your hand's card wins then
;;    card-select: (listof Num) Card -> Card
;; if your hand's card loses then
;;    card-select: (listof Num) Card -> Bool 
;; Examples:

(check-expect (card-select (cons (make-card 1 1 1 1) empty)  (make-card 2 2 2 2)) false)
(check-expect (card-select (cons (make-card 3 3 3 3) empty)  (make-card 2 2 2 2))
              (make-card 3 3 3 3))

(define (card-select hand Card3)
  
  (cond
    [(empty? hand) false]
    [(symbol=? (card-battle (first hand) Card3) 'win) (first hand)]
    [else (card-select (rest hand) Card3)] ))

;;Tests

(check-expect (card-select (cons (make-card 3 3 3 3) empty)  (make-card 4 4 4 4))
              false)

(check-expect (card-select (cons (make-card 9 9 9 9) empty)  (make-card 2 2 2 2))
              (make-card 9 9 9 9))

(define (count-collector-cards hand2)
  (cond [(empty? hand2) 0]
        [(symbol=? (card-regular? (first hand2)) true) (count-collector-cards (rest hand2))]
        [else (+ 1 (count-collector-cards (rest hand2)))]))
