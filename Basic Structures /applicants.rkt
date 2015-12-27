;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname applicants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; *************************************
;;  Hamza Usmani: University of Waterloo
;; *************************************

(define-struct pprof (title wts))
;; A position profile (PProf) is a (make-pprof Sym (listof Nat))

(define (applicant-score lst-skill-rank posprof)
  (cond
    [(empty? lst-skill-rank) 0]
    [else (+ (* (first lst-skill-rank) (first (pprof-wts posprof)))
             (applicant-score (rest lst-skill-rank)
                              (make-pprof (pprof-title posprof)
                                         (rest (pprof-wts posprof)))))]))
  

;; (position-max skill-rank PProf-1 PProf-2) produces the profile
;;     that has the greatest applicant-score
;; position-max: Sym PProf PProf -> PProf
;; A PProf is a (make-pprof Sym (listof Nat))
  
  
  (define (position-max skill-rank PProf-1 PProf-2)
    (cond
      [(empty? skill-rank) 0]
      [(> (applicant-score skill-rank PProf-2)
          (applicant-score skill-rank PProf-1))
       PProf-2]
      [else PProf-1]))

(define (position-list-max lst-skill-rank lst-pospro)
  (cond
   [(empty? (rest lst-pospro)) (first lst-pospro)]
   [else (position-max lst-skill-rank
                       (position-max lst-skill-rank (first lst-pospro)
                       (first (rest lst-skill-rank)))
                       (position-list-max lst-pospro (rest lst-skill-rank)))]))


