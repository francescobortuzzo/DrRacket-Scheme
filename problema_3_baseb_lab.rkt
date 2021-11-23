;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname esercizio_base_b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bin-val ; val intero non negativo
  (lambda (x y) ; b è una stringa di 0/1
     (let((k (- (string-length y) 1))(b (string-length x)))
      (if (= k 0)
          (position x y 0)
          (+ (* b( bin-val x(substring y 0 k)))
                (position x (substring y k) 0)
           ))
      )
    )
  )


(define position           ; val: numero intero /posizione di s in seq
  (lambda (x y i)        ; seq: stringa    s: stringa /un carattere    i: intero /variabile di ciclo    
        (if (string=? y (substring x i (+ i 1)))
            i
            (position x y (+ i 1)))
 ))

(define punto
  (lambda (x)
    (string-append
     (substring x 0 (position x "." 0))
     (substring x (+ 1 (position x "." 0)))
    )
))

(define bin-val1 ; val intero non negativo
  (lambda (x y) ; b è una stringa di 0/1
    (/
       (bin-val x (punto y))
       (expt (string-length x) (string-length(substring y (+ 1 (position y "." 0))))))
 ))


(define punto?
 (lambda (x)
   (cond
     ((= (string-length x) 0) #false)
     ((string=? "." (substring x 0 1)) #true)
      (else (punto? (substring x 1)))
       
 )))

(define rep->number
  (lambda (x y)
    (if (punto? y)
        (cond(( char=? #\+ (string-ref y 0))
                     (bin-val1 x (substring y 1)))
              ((char=? #\- (string-ref y 0))
                     (string-append
                      "-"
                      (number->string(bin-val1 x (substring y 1)))
                      ))
              (else 
                      (bin-val1 x y)
              ))
        (cond(( char=? #\+ (string-ref y 0))
                      (bin-val x (substring y 1)))
              (( char=? #\- (string-ref y 0))
                      (string-append
                       "-"
                       (number->string(bin-val x (substring y 1))))
             )
              (else
                       (bin-val x y)
              )
      )
     )
))
