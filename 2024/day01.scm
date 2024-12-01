(load "../common.scm")

(define (parse input)
  (map-2d string->number (parse-2d input)))

(define (part1 input)
  (let*
    ([parsed (parse input)]
     [zipped (apply zip parsed)]
     [sorted (map (lambda (x) (sort x <)) zipped)]
     [unzipped (apply zip sorted)]
     [difference (map (curry apply -) unzipped)]
     [abs-difference (map abs difference)]
     [result (apply + abs-difference)])
    result))

(define (part2 input)
  (let*
    ([parsed (parse input)]
     [zipped (apply zip parsed)]
     [appearances
       (map
         (lambda (left) (count (curry = left) (cadr zipped)))
         (car zipped))]
     [scores (map (curry apply *) (zip appearances (car zipped)))]
     [result (apply + scores)])
    result))

;(part2 "3   4
;4   3
;2   5
;1   3
;3   9
;3   3
;")
(part2 (read-file "input01.txt"))
