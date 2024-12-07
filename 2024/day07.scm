(load "../common.scm")

(define (parse input)
  (match-let*
    ([lines (split-lines input)]
     [(total-str operands-str)
      (apply zip (map (curry* regexp-split ": ") lines))]
     [total (map string->number total-str)]
     [operands-split (map split-row operands-str)]
     [operands (map-2d string->number operands-split)])
    (zip total operands)))

(define (combinations l n)
  (apply list-product (repeat l n)))

(define (apply-operators operands operators)
  (fold
    (lambda (x acc)
      ((cadr x) acc (car x)))
    (car operands)
    (zip (cdr operands) operators)))

(define (calibrated? total operands operators)
  (let*
    ([len (length operands)]
     [combos (combinations operators (- len 1))]
     [results
       (map
         (lambda (opt) (apply-operators operands opt))
         combos)])
    (member total results)))

(define (part1 input)
  (let*
    ([parsed (parse input)]
     [filtered
       (filter
         (lambda (equation)
           (calibrated? (car equation) (cadr equation) (list + *)))
         parsed)])
    (apply + (map car filtered))))

(define (|| . numbers)
  (string->number (string-concatenate (map number->string numbers))))

(define (part2 input)
  (let*
    ([parsed (parse input)]
     [filtered
       (par-filter
         (lambda (equation)
           (calibrated? (car equation) (cadr equation) (list + * ||)))
         parsed)])
    (apply + (map car filtered))))

;(part2 "190: 10 19
;3267: 81 40 27
;83: 17 5
;156: 15 6
;7290: 6 8 6 15
;161011: 16 10 13
;192: 17 8 14
;21037: 9 7 18 13
;292: 11 6 16 20
;")
(part2 (read-file "input07.txt"))
