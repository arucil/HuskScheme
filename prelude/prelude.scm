;;;
;;; Prelude definitions for HuskScheme
;;;

(define (not x)
  (if x #f #t))

(define (list . xs)
  xs)

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map f (cdr xs)))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs)
            (append (cdr xs) ys))))

(define (Y f)
  ((lambda (g) (g g))
   (lambda (g)
     (lambda (x)
       ((f (g g)) x)))))

;;; Macros
