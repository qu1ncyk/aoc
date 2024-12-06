(load "../common.scm")

(define (parse input)
    (drop-right (string-split input #\lf) 1))

(define (start-pos grid)
  (let*
    ([width (string-length (car grid))]
     [height (length grid)]
     [find-start-col
       (lambda (row-str) (string-index row-str #\^))]
     [start-row (list-index find-start-col grid)]
     [start-col (find-start-col (list-ref grid start-row))])
    (list start-col start-row)))

(define (list+ l1 l2)
  (map (curry apply +) (zip l1 l2)))

(define (rotate90 pos)
  (match-let
    ([(x y) pos])
    (list (- 0 y) x)))

(define (obstacle? grid pos)
  (match-let*
    ([width (string-length (car grid))]
     [height (length grid)]
     [(x y) pos])
    (if (and (< -1 x width) (< -1 y height))
      (equal? #\# (string-ref (list-ref grid y) x))
      #t)))

(define (walk history grid pos dir)
  (match-let*
    ([width (string-length (car grid))]
     [height (length grid)]
     [pos' (list+ pos dir)]
     [dir' (rotate90 dir)]
     [history' (cons pos history)]
     [(x y) pos'])
    (cond
      [(not (and (< -1 x width) (< -1 y height))) history]
      [(obstacle? grid pos') (walk history' grid (list+ pos dir') dir')]
      [else (walk history' grid pos' dir)])))

(define (part1 input)
  (let*
    ([parsed (parse input)]
     [start (start-pos parsed)]
     [history (walk '() parsed start '(0 -1))])
    (+ 1 (length (list-unique history)))))

(define (walk* history grid pos dir)
  (match-let*
    ([width (string-length (car grid))]
     [height (length grid)]
     [pos' (list+ pos dir)]
     [dir' (rotate90 dir)]
     [state (list pos dir)]
     [history' (vhash-cons state #t history)]
     [(x y) pos'])
    (cond
      [(vhash-assoc state history) (cons #f history')]
      [(not (and (< -1 x width) (< -1 y height))) history']
      [(obstacle? grid pos') (walk* history' grid pos dir')]
      [else (walk* history' grid pos' dir)])))

(define (place-obstacle grid pos)
  (match-let*
    ([(x y) pos]
     [row (list-ref grid y)]
     [row' (string-set row x #\#)])
    (list-set grid y row')))

(define (find-obstacle-places grid history)
  (let
    ([start (start-pos grid)]
     [pos-list
       (vhash-fold
         (lambda (state _ acc) (cons (car state) acc))
         '()
         history)])
    (par-filter
      (lambda (pos)
        (let
          ([grid' (place-obstacle grid pos)])
          (and
            (not (equal? pos start))
            (pair?
              (walk* (alist->vhash '()) grid' start '(0 -1))))))
      pos-list)))

(define (part2 input)
  (let*
    ([parsed (parse input)]
     [start (start-pos parsed)]
     [history (walk* (list->vlist '()) parsed start '(0 -1))]
     [obstacle-places (find-obstacle-places parsed history)])
    (length (list-unique obstacle-places))))

;(part2 "....#.....
;.........#
;..........
;..#.......
;.......#..
;..........
;.#..^.....
;........#.
;#.........
;......#...
;")
(part2 (read-file "input06.txt"))
