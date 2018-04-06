;;;
;;; Prelude definitions for HuskScheme
;;;

;;;;;;;;;;;;     Macros    ;;;;;;;;;;;;;;;;;;

(defmacro (quasiquote exp)
  (define (expand exp lv)
    (cond
     []))
  (expand exp 0))

;;;;;;;;;;;;    Functions   ;;;;;;;;;;;;;;;;;

(define (caar x)
  (car (car x)))
(define (cadr x)
  (car (cdr x)))
(define (cdar x)
  (cdr (car x)))
(define (cddr x)
  (cdr (cdr x)))

(define (caaar x)
  (car (car (car x))))
(define (caadr x)
  (car (car (cdr x))))
(define (cadar x)
  (car (cdr (car x))))
(define (caddr x)
  (car (cdr (cdr x))))
(define (cdaar x)
  (cdr (car (car x))))
(define (cdadr x)
  (cdr (car (cdr x))))
(define (cddar x)
  (cdr (cdr (car x))))
(define (cdddr x)
  (cdr (cdr (cdr x))))

(define (caaaar x)
  (car (car (car (car x)))))
(define (caaadr x)
  (car (car (car (cdr x)))))
(define (caadar x)
  (car (car (cdr (car x)))))
(define (caaddr x)
  (car (car (cdr (cdr x)))))
(define (cadaar x)
  (car (cdr (car (car x)))))
(define (cadadr x)
  (car (cdr (car (cdr x)))))
(define (caddar x)
  (car (cdr (cdr (car x)))))
(define (cadddr x)
  (car (cdr (cdr (cdr x)))))
(define (cdaaar x)
  (cdr (car (car (car x)))))
(define (cdaadr x)
  (cdr (car (car (cdr x)))))
(define (cdadar x)
  (cdr (car (cdr (car x)))))
(define (cdaddr x)
  (cdr (car (cdr (cdr x)))))
(define (cddaar x)
  (cdr (cdr (car (car x)))))
(define (cddadr x)
  (cdr (cdr (car (cdr x)))))
(define (cdddar x)
  (cdr (cdr (cdr (car x)))))
(define (cddddr x)
  (cdr (cdr (cdr (cdr x)))))

(define (not x)
  (if x #f #t))

(define (list . xs)
  xs)

(define (foldr f x0 xs)
  (if (null? xs)
      x0
      (f (car xs)
         (foldr f x0 (cdr xs)))))

(define (foldl f x0 xs)
  (if (null? xs)
      x0
      (foldl f
             (f x0 (car xs))
             (cdr xs))))

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