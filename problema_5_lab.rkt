;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_5_lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define manhattan       ;indica il numero n di percorsi possibili da P(0,0) a P'(i,j)
  (lambda(i j)
    (if (and (> i 0) (> j 0))
        (+ (manhattan (- i 1) j) (manhattan i (- j 1)))
        1
    )
  ))

(define manhattan_3D    ;indica il numero n di percorsi possibili da P(0,0,0) a P'(i,j,k)
  (lambda (i j k)
   (if (and (> i 0) (> j 0) (> k 0))
       (+ (manhattan_3D (- i 1) j k) (manhattan_3D i (- j 1) k) (manhattan_3D i j (- k 1)))     
       (cond ((and (= i 0) (= j 0)) 1)
             ((and (= i 0) (= k 0)) 1)
             ((and (= k 0) (= j 0)) 1)
             ((and (= i 0)(> j 0)(> k 0)) (manhattan j k))
             ((and (= j 0)(> i 0)(> k 0)) (manhattan i k))
             ((and (= k 0)(> j 0)(> i 0)) (manhattan j i))         
       )
    )
 ))