(load "../common.scm")

(define (part1 input)
  (let*
    ([muls
       (map match:substring
            (list-matches "mul\\([0-9]{1,3},[0-9]{1,3}\\)" input))]
     [pairs
       (map
         (lambda(mul)
           (map match:substring (list-matches "[0-9]{1,3}" mul)))
         muls)]
     [numbers (map-2d string->number pairs)])
    (apply + (map (curry apply *) numbers))))

(define (part2 input)
  (let*
    ([muls
       (map match:substring
            (list-matches "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do(n't)?\\(\\)" input))]
     [filtered
       (fold
         (lambda (x acc)
           (cond
             [(equal? x "do()")
              `(#t . ,(cdr acc))]
             [(equal? x "don't()")
              `(#f . ,(cdr acc))]
             [(car acc)
              `(#t . ,(cons x (cdr acc)))]
             [else `(#f . ,(cdr acc))]))
         '(#t . ()) muls)]
     [muls (cdr filtered)]
     [pairs
       (map
         (lambda(mul)
           (map match:substring (list-matches "[0-9]{1,3}" mul)))
         muls)]
     [numbers (map-2d string->number pairs)]
     [total (apply + (map (curry apply *) numbers))])
    total))

;(part2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(part2 (read-file "input03.txt"))
