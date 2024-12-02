(load "../common.scm")

(define (parse input)
  (map-2d string->number (parse-2d input)))

(define (gradual? l)
  (let*
    ([pairs (zip (drop-right l 1) (cdr l))])
    (every
      (lambda (p)
        (let
          ([diff (abs (apply - p))])
          (and (>= diff 1) (<= diff 3))))
      pairs)))

(define (part1 input)
  (let*
    ([parsed (parse input)]
     [increasing (filter (curry apply <) parsed)]
     [decreasing (filter (curry apply >) parsed)]
     [joined (concatenate (list increasing (map reverse decreasing)))]
     [gradual (filter gradual? joined)])
    (length gradual)))


(define (line-safe? line)
  (let
    ([increasing (apply < line)]
     [decreasing (apply > line)])
    (cond
      [increasing (gradual? line)]
      [decreasing (gradual? (reverse line))]
      [else #f])))

(define (without-index l i)
  (concatenate
    (list
      (take l i)
      (drop l (+ i 1)))))

(define (tolerant-safe? line)
  (let
    ([indices (iota (length line))])
    (any
      (lambda (i) (line-safe? (without-index line i)))
      indices)))

(define (part2 input)
  (let*
    ([parsed (parse input)]
     [gradual (filter tolerant-safe? parsed)])
    (length gradual)))

;(part2 "7 6 4 2 1
;1 2 7 8 9
;9 7 6 2 1
;1 3 2 4 5
;8 6 4 4 1
;1 3 6 7 9
;")
(part2 (read-file "input02.txt"))
