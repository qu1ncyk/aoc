(load "../common.scm")

(define (parse str)
  (filter
    (lambda (x) (and (>= x 0) (<= x 9)))
    (map
      (lambda (x) (- (char->integer x) 48))
      (string->list str))))

(define (rotate ls)
  (append (cdr ls) (list (car ls))))

(define (rotate-halfway ls)
  (let*
    ([len (length ls)]
     [half-len (/ len 2)])
    (append (drop ls half-len) (take ls half-len))))

(define (tuple-eq? x)
  (= (car x) (cadr x)))

(define (part1 input-str)
  (let*
    ([input (parse input-str)]
     [rotated (rotate input)]
     [zipped (zip input rotated)]
     [filtered (filter tuple-eq? zipped)]
     [singular (map car filtered)])
    (fold + 0 singular)))

(define (part2 input-str)
  (let*
    ([input (parse input-str)]
     [rotated (rotate-halfway input)]
     [zipped (zip input rotated)]
     [filtered (filter tuple-eq? zipped)]
     [singular (map car filtered)])
    (fold + 0 singular)))

(part2 (read-file "input01.txt"))
