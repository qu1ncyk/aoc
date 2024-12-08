(load "../common.scm")

(define (parse input)
  (let*
    ([lines (split-lines input)])
    (grid-parse lines)))

(define (pairwise proc p1 p2)
  (cons
    (proc (car p1) (car p2))
    (proc (cdr p1) (cdr p2))))

(define (distance p1 p2)
  (match-let*
    ([d (pairwise - p1 p2)]
     [(d1 . d2) d])
    (if
    (or
      (< d1 0)
      (and (= d1 0) (< d2 0)))
    (cons (- 0 d1) (- 0 d2))
    (cons d1 d2))))

(define (antinode? grid pos frequency)
  (let*
    ([antenna-pos (2w-dict-ref-val* frequency grid)]
     [distances
       (filter
         (curry (proc-concat not equal?) '(0 . 0))
         (map (curry distance pos) antenna-pos))]
     [2distances
      (map
        (lambda (d) (pairwise * d '(2 . 2)))
        distances)])
    (not (null? (lset-intersection equal? distances 2distances)))))

(define (antinodes-of-freq grid' frequency)
  (match-let*
    ([($ grid dict width height) grid']
     [coords (list-product2 (iota width) (iota height))])
    (filter
      (lambda (c) (antinode? dict c frequency))
      coords)))

(define (part1 input)
  (let*
    ([parsed (parse input)]
     [frequencies
       (filter (lambda (x) (not (equal? #\. x)))
         (list-unique
           (map cdr
             (2w-dict->alist
              (grid-dict parsed)))))]
     [antinodes-per-freq (map (curry antinodes-of-freq parsed) frequencies)])
    (length (list-unique (concatenate antinodes-per-freq)))))

;(part1 "............
;........0...
;.....0......
;.......0....
;....0.......
;......A.....
;............
;............
;........A...
;.........A..
;............
;............
;")
(part1 (read-file "input08.txt"))
