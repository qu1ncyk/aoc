(use-modules (ice-9 textual-ports))
(use-modules (ice-9 match))
(use-modules (ice-9 threads))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-1))

(define (read-file filename)
  (call-with-input-file filename get-string-all))

(define (curry f . args)
  (lambda x (apply f (append args x))))

(define (curry* f . args)
  (lambda x (apply f (append x args))))

(define (proc-concat . procs)
  (lambda x
    (fold-right
      (lambda (f acc) (f acc))
      (apply (last procs) x)
      (drop-right procs 1))))

(define (split-lines str)
  (drop-right (string-split str #\lf) 1))

(define (split-row str)
  (filter
    (lambda (x) (not (equal? x "")))
    (concatenate
      (map (lambda (x) (string-split x #\sp))
           (string-split str #\tab)))))

(define (parse-2d input)
  (map split-row (split-lines input)))

(define (map-2d f 2d-list)
  (map (curry map f) 2d-list))

(define-syntax debug
  (syntax-rules ()
    [(debug . expr)
     (begin
       (display 'expr)
       (display " = ")
       (display expr)
       (newline)
       expr)]))

(define (regexp-split str regexp)
  (let*
    ([placeholder "€"]
     [replaced
       (regexp-substitute/global #f regexp str 'pre placeholder 'post)]
     [split (string-split replaced (string->char-set placeholder))])
    split))

(define (list-unique l)
  (fold
    (lambda (x acc)
      (if (member x acc)
        acc
        (cons x acc)))
    '()
    l))

(define (list-set l i x)
  (let
    ([front (take l i)]
     [back (drop l (+ i 1))])
    (append front (list x) back)))

(define (string-set s i x)
  (let*
    ([l (string->list s)]
     [l' (list-set l i x)])
    (list->string l')))

(define (par-filter p l)
  (let*
    ([bools (par-map p l)]
     [zipped (zip l bools)]
     [filtered (filter cadr zipped)])
    (map car filtered)))

(define (list-product2 l1 l2)
  (concatenate
    (map
      (lambda (x1)
        (map
          (lambda (x2) (cons x1 x2))
          l2))
      l1)))

(define (list-product . l)
  (fold-right list-product2 '(()) l))

(define (repeat x n)
  (map (const x) (iota n)))
