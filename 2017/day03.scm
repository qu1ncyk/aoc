(load "../common.scm")

(define (prev-odd-root n)
  (let*
    ([root (floor (sqrt n))]
     [odd-root (if (odd? root) root (- root 1))])
    odd-root))

(define (calculate-coords' prev-root coords to-go)
  (cond
    [(zero? to-go) coords]

    ; On the right side of the square, go up
    [(and (= (+ prev-root 1)
             (car coords))
          (>= (cadr coords)
             (- 0 prev-root)))
     (calculate-coords'
       prev-root
       (list (car coords) (- (cadr coords) 1))
       (- to-go 1))]

    ; On the top side of the square, go left
    [(and (= (- 0 (+ prev-root 1))
             (cadr coords))
          (>= (car coords)
             (- 0 prev-root)))
     (calculate-coords'
       prev-root
       (list (- (car coords) 1) (cadr coords))
       (- to-go 1))]

    ; On the left side of the square, go down
    [(and (= (- 0 (+ prev-root 1))
             (car coords))
          (<= (cadr coords)
             (- 0 prev-root)))
     (calculate-coords'
       prev-root
       (list (car coords) (+ (cadr coords) 1))
       (- to-go 1))]

    ; On the bottom side of the square, go right
    [else
     (calculate-coords'
       prev-root
       (list (+ (car coords) 1) (cadr coords))
       (- to-go 1))]))

(define (calculate-coords n)
  (let*
    ([root (prev-odd-root n)]
     [coord (/ (- root 1) 2)]
     [coords (list (+ coord 1) coord)])
    (calculate-coords' coord coords (- n (expt root 2) 1))))

(define (part1 input)
  (apply + (map abs (calculate-coords input))))

(part1 361527)
