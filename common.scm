(use-modules (ice-9 textual-ports))
(use-modules (ice-9 match))
(use-modules (ice-9 threads))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))

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

(define (enumerate l)
  (map
    (lambda (x) (cons (car x) (cadr x)))
    (zip (iota (length l)) l)))

(define (enumerate-2d l)
  (apply zip (map enumerate (apply zip (map enumerate l)))))

(define (list->pair l)
  (cons (car l) (cadr l)))

(define (make-2w-dict)
  (cons (alist->vhash '()) (alist->vhash '())))

(define (2w-dict-cons key val dict)
  (cons
    (vhash-cons key val (car dict))
    (vhash-cons val key (cdr dict))))

(define (2w-dict-ref-key key dict)
    (cdr (vhash-assoc key (car dict))))

(define (2w-dict-ref-val val dict)
    (cdr (vhash-assoc val (cdr dict))))

(define (2w-dict-ref-key* key dict)
    (vhash-fold* cons '() key (car dict)))

(define (2w-dict-ref-val* val dict)
    (vhash-fold* cons '() val (cdr dict)))

(define (2w-dict->alist dict)
  (vhash-fold
    (lambda (key val acc) (cons (cons key val) acc))
    '()
    (car dict)))

(define-immutable-record-type grid
  (make-grid dict width height)
  grid?
  (dict grid-dict set-grid-dict)
  (width grid-width set-grid-width)
  (height grid-height set-grid-height))

(define (grid-parse lines)
  (let*
    ([chars (map string->list lines)]
     [height (length chars)]
     [width (length (car chars))]
     [dict
       (fold
         (match-lambda* [((y x . c) dict)
           (2w-dict-cons (cons x y) c dict)])
         (make-2w-dict)
         (concatenate (enumerate-2d chars)))])
    (make-grid dict width height)))
