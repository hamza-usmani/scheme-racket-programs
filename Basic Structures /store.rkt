;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname store) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; A Product is a (make-product Sym Num Bool)
;; requires: price > 0,
;; price cannot have fractional cents
(define-struct product (name price taxable?))


;; (shop Product) is a function that contains a list of
;;    the structure type Product
;; A Shop is a (make-product Sym Nat Bool)
;; shop: Product -> (listof Product)
;; Examples:

(check-expect (shop (make-product  'can 6 true))
              (cons (make-product 'can 6 true) empty))

(check-expect (shop (make-product  'paper 8 false))
              (cons (make-product 'paper 8 false) empty))

(define (shop Product)
  (cons Product empty))

(check-expect (shop (make-product 'rocket 2 false)) (list (make-product 'rocket 2 false)))

;; (have-product? item Shop) checks if a symbol of a part
;;    of a shopping list, is actually in the product list
;; have-product?: Sym (listof Product) -> Bool 
;; Examples:

(check-expect (have-product? 'pan (cons (make-product 'car 1 true)
                                        (cons (make-product 'butter 5 false)
                                              empty))) false)

(check-expect (have-product? 'pencil (cons (make-product 'pencil 1 true)
                                           (cons (make-product 'laptop 5 false)
                                                 empty))) true)

(check-expect (have-product? 'ball (cons (make-product 'car 1 true)
                                         (cons (make-product 'ball 5 false)
                                               empty))) true)

(define (have-product? item Shop)
  (cond [(empty? Shop) false]
        [(symbol=? item (product-name (first Shop))) true]
        [else (have-product? item (rest Shop))]))

;;Tests

(check-expect (have-product? 'rocket
                             empty) false)

(check-expect (have-product? 'rocket
                             (list (make-product 'rocket 2 false))) true)

(check-expect (have-product? 'candy
                             (list (make-product 'rocket 2 false)
                                   (make-product 'candy 2 false))) true) 

;; (countlist lst) counts the number of items in the list
;; countlist: (listof Any) -> Nat
;; Examples:
(check-expect (countlist (list 2 3 4)) 3)

(define (countlist lst)
  (cond [(empty? lst) 0]
        [else (+ 1 (countlist (rest lst)))]))

;;Tests
(check-expect (countlist (list 2 3 4 5)) 4)
(check-expect (countlist(list 1)) 1)


;; (valid-order-num all Shop) checks if elements
;;    of a list are put of the list Shop
;; valid-order-num: Sym Shop -> Nat
;; A Shop is a (make-product Sym Nat Bool)
;;Examples

(check-expect (valid-order-num (list 'a 'b 'c 'd) (list (make-product 'd 2 false))) 1)

(define (valid-order-num all Shop)
  (cond [(empty? all) 0]
        [(empty? Shop) 0]
        [(symbol=? (first all) (product-name (first Shop)))
         (+ 1 (valid-order-num (rest all) (rest Shop)))]
        [else (valid-order-num (rest all)  Shop)]))

;;Tests
(check-expect (valid-order-num (list 'a 'b 'c) (list (make-product 'a 2 false)
                                                     (make-product 'b 2 false))) 2)

(check-expect (valid-order-num (list 'a 'b 'c) (list (make-product 'a 2 false))) 1)

(check-expect (valid-order-num (list 'b 'd 'a) (list (make-product 'a 2 false))) 1)

(check-expect (valid-order-num (list 'b 'd 'a) (list (make-product 'a 2 false))) 1)


;; (valid-order? allitems Shop) checks if the list is part of
;;   the Shop list
;; valid-order?: (listof Sym) Shop -> Bool
;; A Shop is a (make-product Sym Nat Bool)
;;Examples

(check-expect (valid-order? (list 'c 'b 'a) (list (make-product 'c 2 false))) false)

(define (valid-order? allitems Shop)
  (cond [(= (valid-order-num allitems Shop) (countlist allitems)) true]
        [else false]))

;;Tests
(check-expect (valid-order? (list 'a 'b 'c) (list (make-product 'a 2 false)
                                                  (make-product 'b 2 false)
                                                  (make-product 'c 2 false))) true)

(check-expect (valid-order? (list 'a 'b 'c) (list (make-product 'a 2 false))) false)


(define tax 1.13)

;; (budget-items Shop limit) produces the list of products
;;   with a final sum price of less than or equal to the price limit
;; budget-items: Shop Nat -> Nat
;; A Shop is a (make-product Sym Nat Bool)
;; Examples:

(check-expect (budget-items (list (make-product 'a 6 false)
                                  (make-product 'b 7 false)
                                  (make-product 'c 9 false)) 10)
              (list (make-product 'a 6 false)
                    (make-product 'b 7 false)))

(define (budget-items Shop limit)
  
  (cond
    [(empty? Shop) empty]
    [(> (* (product-price (first Shop)) tax) limit)
     (budget-items (rest Shop) limit)]
    [else (cons (first Shop) (budget-items (rest Shop) limit))]))

;;Tests
(check-expect (budget-items (list (make-product 'a 2 false)
                                  (make-product 'b 5 false)
                                  (make-product 'c 100 false)) 10)
              (list (make-product 'a 2 false)
                    (make-product 'b 5 false)))

(check-expect (budget-items (list (make-product 'a 9 false)
                                  (make-product 'b 9 false)
                                  (make-product 'c 1 false)) 10)
              (list (make-product 'c 1 false)))

; (total-order lstitems Shop) produces the final cost of the order
;;     including tax
;; total-order: (listof Sym) Shop -> Num
;; A Shop is a (make-product Sym Nat Bool)
;; Examples:

(check-expect (total-order (list 'car 'house 'dog)
                           (list (make-product 'car 20 false)
                                 (make-product 'house 75 false)
                                 (make-product 'dog 75 false))) 170)

(define (total-order listitems Shop)
  (cond [(empty? listitems) 0]
        [(empty? Shop) 0]
        [(symbol=? (first listitems) (product-name (first Shop)))
         (+ (product-price (first Shop)) (total-order (rest listitems) Shop))]
        [else (total-order listitems (rest Shop))]))

;;Tests
(check-expect (total-order (list 'a 'b 'c)
                           (list (make-product 'a 50 false)
                                 (make-product 'b 50 false)
                                 (make-product 'c 100 false))) 200)




