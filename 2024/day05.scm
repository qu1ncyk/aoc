(load "../common.scm")

(define (parse input)
  (match-let*
    ([(ordering-str update-str) (regexp-split input "\n\n")]
     [ordering (string-split ordering-str #\lf)]
     [order-pairs (map (lambda (s) (string-split s #\|)) ordering)]
     [updates (drop-right (string-split update-str #\lf) 1)]
     [update-lists (map (lambda (s) (string-split s #\,)) updates)])
    (list
      (map-2d string->number order-pairs)
      (map-2d string->number update-lists))))

(define (in-order? orders update)
  (if (null? update)
    #t
    (match-let*
      ([pair-in-order?
        (lambda (p)
          (not (member (reverse p) orders)))]
       [(head . tail) update]
       [first-in-order
         (every
           (lambda (t) (pair-in-order? (list head t)))
           tail)])
      (and first-in-order (in-order? orders tail)))))

(define (part1 input)
  (match-let*
    ([(order-pairs update-lists) (parse input)]
     [filtered
       (filter (lambda (u) (in-order? order-pairs u))
               update-lists)]
     [middle
       (map
         (lambda (l)
           (let
             ([index (/ (- (length l) 1) 2)])
             (list-ref l index)))
            filtered)])
    (apply + middle)))

(define (reorder orders update)
  (if (null? (cdr update))
    update
    (let*
      ([relevant-orders
         (filter
           (lambda (order)
             (every (lambda (o) (member o update)) order))
           orders)]
       [right-values (cadr (apply zip relevant-orders))]
       [smallest
         (find
           (lambda (v) (not (member v right-values)))
           update)]
       [update' (remove (curry = smallest) update)])
      (cons smallest (reorder relevant-orders update')))))

(define (part2 input)
  (match-let*
    ([(order-pairs update-lists) (parse input)]
     [filtered
       (filter (lambda (u) (not (in-order? order-pairs u)))
               update-lists)]
     [reordered (map (curry reorder order-pairs) filtered)]
     [middle
       (map
         (lambda (l)
           (let
             ([index (/ (- (length l) 1) 2)])
             (list-ref l index)))
            reordered)])
    (apply + middle)))

;(part2 "47|53
;97|13
;97|61
;97|47
;75|29
;61|13
;75|53
;29|13
;97|29
;53|29
;61|53
;97|53
;61|29
;47|13
;75|47
;97|75
;47|61
;75|61
;47|29
;75|13
;53|13
;
;75,47,61,53,29
;97,61,53,29,13
;75,29,13
;75,97,47,61,53
;61,13,29
;97,13,75,29,47
;")
(part2 (read-file "input05.txt"))
