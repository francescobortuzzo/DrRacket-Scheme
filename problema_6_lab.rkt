;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problema_6_lab) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
(define L-tessellation ;costruisce ricorsivamente la figura L-tile
  (lambda (x) ; potenza di 2
    (cond ((= x 1) L-tile)
          (else
           (glue-tiles
            (glue-tiles
             (L-tessellation (/ x 2))
             (shift-down (shift-right (L-tessellation (/ x 2)) (*(/ x 2) 0.5)) (*(/ x 2) 0.5))
             )
            (glue-tiles
             (shift-down (quarter-turn-left (L-tessellation (/ x 2)))(*(/ x 2)))
             (shift-right (quarter-turn-right (L-tessellation (/ x 2)))(*(/ x 2)))
             )
            )
          )   
    )
))
