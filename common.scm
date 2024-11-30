(use-modules (ice-9 textual-ports))
(use-modules (srfi srfi-1))

(define (read-file filename)
  (call-with-input-file filename get-string-all))

(define (curry f . args)
  (lambda x (apply f (concatenate (list args x)))))

(define (proc-concat . procs)
  (lambda x
    (fold-right
      (lambda (f acc) (f acc))
      (apply (last procs) x)
      (drop-right procs 1))))

(define (split-lines str)
  (drop-right (string-split str #\lf) 1))
