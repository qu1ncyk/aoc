(load "../common.scm")

(define (parse input)
  (split-lines input))

(define (transpose strings)
  (map list->string (apply zip (map string->list strings))))

(define (rotate90 strings)
  (mirror (transpose strings)))

(define (rotate180 strings)
  (rotate90 (rotate90 strings)))

(define (diagonal strings x-start y-start)
  (let*
    ([width (string-length (car strings))]
     [height (length strings)]
     [diag-size (min (- width x-start) (- height y-start))])
    (list->string
      (map
        (lambda (i)
          (string-ref (list-ref strings (+ i y-start)) (+ i x-start)))
        (iota diag-size)))))

(define (mirror strings)
  (map string-reverse strings))

(define (count-string regex strings)
  (apply
    +
    (map
      (lambda (s) (length (list-matches regex s)))
      strings)))

(define (diagonals strings)
  (let*
    ([width (string-length (car strings))]
     [height (length strings)]
     [diag-down
       (map
         (lambda (y) (diagonal strings 0 y))
         (iota height))]
     [diag-right
       (map
         (lambda (x) (diagonal strings (+ x 1) 0))
         (iota (- width 1)))])
    (append diag-down diag-right)))

(define (part1 input)
  (let*
    ([parsed (parse input)]
     [diag (diagonals parsed)]
     [r (count-string "XMAS" parsed)]
     [l (count-string "XMAS" (rotate180 parsed))]
     [d (count-string "XMAS" (rotate90 parsed))]
     [u (count-string "XMAS" (rotate90 (rotate180 parsed)))]
     ; Direction names are not necessarily correct
     [rd (count-string "XMAS" (diagonals parsed))]
     [ld (count-string "XMAS" (diagonals (rotate180 parsed)))]
     [ru (count-string "XMAS" (diagonals (rotate90 parsed)))]
     [lu (count-string "XMAS" (diagonals (rotate90 (rotate180 parsed))))])
    (+ r l d u rd ld ru lu)))

(define (block strings x y)
  (let
    ([rows (take (drop strings y) 3)])
    (map (lambda (s) (substring s x (+ x 3))) rows)))

(define (blocks strings)
  (let*
    ([width (string-length (car strings))]
     [height (length strings)])
    (concatenate
      (map
        (lambda (x)
          (map
            (lambda (y)
              (block strings x y))
            (iota (- height 2))))
        (iota (- width 2))))))

(define (x-mas? block)
  (let*
    ([x-mas
       '(("M.M"
          ".A."
          "S.S")
         ("S.M"
          ".A."
          "S.M")
         ("S.S"
          ".A."
          "M.M")
         ("M.S"
          ".A."
          "M.S"))]
     [match
       (any
         (lambda(patterns)
         (every
           (lambda (x) (string-match (car x) (cadr x)))
           (zip patterns block))) x-mas)]
     )
    match))

(define (part2 input)
  (let*
    ([parsed (parse input)])
    (count x-mas? (blocks parsed))))

;(part2 ".M.S......
;..A..MSMS.
;.M.S.MAA..
;..A.ASMSM.
;.M.S.M....
;..........
;S.S.S.S.S.
;.A.A.A.A..
;M.M.M.M.M.
;..........
;")
(part2 (read-file "input04.txt"))
