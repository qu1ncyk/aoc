(use-modules (ice-9 textual-ports))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (read-file filename)
  (call-with-input-file filename get-string-all))

(define (curry f . args)
  (lambda x (apply f (append args x))))

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
