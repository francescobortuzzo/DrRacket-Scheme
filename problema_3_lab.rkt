;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname esercizio_binari) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define neg?
 (lambda (x)      ; : stringa
    (string=? (substring x 0 1) "-")
 ))

(define bin-val ; val intero non negativo
  (lambda (b) ; b è una stringa di 0/1
    (let((k (- (string-length b) 1)))
      (if (= k 0)
          (val b)
          (+ (* 2( bin-val (substring b 0 k)))
                (val(substring b k))
           ))
      )
    )
  )

(define bin-val1
  (lambda (n)
    (bin-val(number->string(* (abs(string->number n)) (expt 10 (- (string-length n) 1)))))
 ))


(define val
  (lambda (b)
    (if (string=? b "0") 0 1)
  )
 )

(define bin-rep->number
  (lambda (x)
    (if(neg? x)
          (* -1 (/ (bin-val1 x) (expt 2 (-(string-length x)1))))
       (/ (bin-val1 x) (expt 2 (-(string-length x)1)))
     )
 ))