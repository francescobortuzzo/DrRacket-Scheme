;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 20212610_lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define articolo
  (lambda (x)
    (let ((k (string-ref x (- (string-length x) 1 ))))
     (cond
      ((char=? k #\o) "il")
      ((char=? k #\i) "i")
      ((char=? k #\a) "la")
      ((char=? k #\e) "le")
    ))
 ))

(define verbo
  (lambda (y x)
    (string-append (substring y 0 (- (string-length y) 3))
    (let ((k (string-ref y (- (string-length y) 3 ))))
      (cond
        ((char=? k #\a)
         (if (sing? x)"a" "ano"))
        ((char=? k #\e)
         (if (sing? x) "e" "ono"))
        ((char=? k #\i)
         (if (sing? x) "e" "ono"))
        )
    ))
 ))

(define sing?
  (lambda (x)      ; : stringa
    (or
      (string=? (substring x (- (string-length x) 1)) "o")
      (string=? (substring x (- (string-length x) 1)) "a")
    )
 ))

(define frase
  (lambda (sogg ver ogg)
    (string-append
     (articolo sogg )
     " "
     sogg
     " "
     (verbo ver sogg)
     " "
     (articolo ogg)
     " "
     ogg
     "."
    )
 ))