(load "../common.scm")

(define (row-checksum row)
  (let
    ([low (apply min row)]
     [high (apply max row)])
    (- high low)))

(define (table-checksum table)
  (let*
    ([row-checksums (map row-checksum table)]
     [checksum (apply + row-checksums)])
    checksum))

(define (part1 input)
  (let*
    ([lines (split-lines input)]
     [table-str (map (lambda (line) (string-split line #\tab)) lines)]
     [table (map (curry map string->number) table-str)])
    (table-checksum table)))


(define (list-product ls1 ls2)
  (concatenate
    (map
      (lambda (x)
        (map (lambda (y) (list x y)) ls2))
        ls1)))

(define (row-div-checksum row)
  (let*
    ([pairs (list-product row row)]
     [divisible-numbers
       (filter
         (lambda (x)
           (and
             (not (apply = x))
             (or
               (= 0 (apply modulo x))
               (= 0 (apply modulo (reverse x))))))
         pairs)]
     [div-pair (car divisible-numbers)]
     [low (apply min div-pair)]
     [high (apply max div-pair)])
    (/ high low)))

(define (table-div-checksum table)
  (let*
    ([row-checksums (map row-div-checksum table)]
     [checksum (apply + row-checksums)])
    checksum))

(define (part2 input)
  (let*
    ([lines (split-lines input)]
     [table-str (map (lambda (line) (string-split line #\tab)) lines)]
     [table (map (curry map string->number) table-str)])
    (table-div-checksum table)))

;(table-checksum '((5 1 9 5)
;                  (7 5 3)
;                  (2 4 6 8)))

;(table-div-checksum '((5 9 2 8)
;                  (9 4 7 3)
;                  (3 8 6 5)))

(part2 (read-file "input02.txt"))
