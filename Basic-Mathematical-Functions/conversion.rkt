;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

;; This small program calculates conversions between units of measures

(define oz->lb 1/16)
(define lb->gram  453.59237) 

(define (oz->gram x)
  (* x oz->lb lb->gram)) 


(define ml->l 1/1000)
(define l->gallon (expt 3.78541 -1))
(define gallon->floz 128)

(define (ml->floz y) 
  (* y ml->l l->gallon gallon->floz ))   


(define (opo->gpl z)
  (* z (/ (* oz->lb lb->gram) (*(expt gallon->floz -1) (expt l->gallon -1)))))     