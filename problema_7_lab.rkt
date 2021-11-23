;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_7_lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define belong?  ;verifica la presenza di un numero intero in una lista
  (lambda (x y)  ;x è un int, y è una lista
    (if (null? y)
        #f
          (if (= x (car y))
              #t
              (belong? x (cdr y))
          )
    )
))

(define position  ;restituisce la posizione di un numero intero in una lista
  (lambda (x y)   ; x è l'intero di cui si vuole sapere la posizione, y è una lista
           (if (= x (car y))
               0
               (+ 1 (position x (cdr y)))
          )
))

(define sorted-ins  ;restituisce la stringa ordinata con all'interno x
  (lambda (x y)     ; x è un intero, y è una lista ordinata
    (if (null? y)
        (cons x y)
        (if (belong? x y)
            (sorted-ins x (eq x y))
            (if (< x (car y))
                (cons x y)
                (append
                   (list (car y))
                   (sorted-ins x (cdr y))
                 )
             )
        )
    )
))

(define sorted-list    ;restituisce la stringa ordinata
  (lambda (y)          ;y è una lista non ordinata
    (if (null? y)
        null
        (sorted-ins (car y) (sorted-list (cdr y)))
    )
))

(define eq            ;elimina l'elemento della stringa uguale a x
  (lambda (x y)       ;x è un intero, y è una lista
    (if (= x (car y))
        (cdr y)
        (append
           (list(car y))
           (eq x (cdr y))
        )
     )
))
