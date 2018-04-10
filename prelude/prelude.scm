;;;
;;; Prelude definitions for HuskScheme
;;;

;;;;;;;;;;;;     Macros    ;;;;;;;;;;;;;;;;;;

(defmacro (let bindings . body)
  `((lambda ,(map car bindings)
      . ,body)
    . ,(map cadr bindings)))

(defmacro (and . vals)
  (if (null? vals)
      #t
      (if (null? (cdr vals))
          (car vals)
          `(if ,(car vals)
               (and . ,(cdr vals))
               #f))))

(defmacro (or . vals)
  (if (null? vals)
      #f
      (if (null? (cdr vals))
          (car vals)
          (let ([t (gensym)])
            `(let ([,t ,(car vals)])
               (if ,t
                   ,t
                   (or . ,(cdr vals))))))))

(defmacro (cond clause . clauses)
  (define rest
    (if (null? clauses)
        '(if #f #f)
        (cond . ,clauses)))
  (if (null? (cdr clause)) ;; [test]
      (let ([t (gensym)])
        `(let ([,t ,(car clause)])
           (if ,t
               ,t
               ,rest)))
      (if (and (null? clauses) ;; [else exp1 exp ...]
               (eq? 'else (car clause)))
          `(begin . ,(cdr clause))
          (if (and (= 3 (length clause))
                   (eq? '=> (cadr clause)))
              (let ([t (gensym)])
                `(let ([,t ,(car clause)])
                   (if ,t
                       (,(caddr clause) ,t)
                       ,rest)))
              `(if ,(car clause)
                   (begin . ,(cdr clause))
                   ,rest)))))

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

(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

(define (Y f)
  ((lambda (g) (g g))
   (lambda (g)
     (lambda (x)
       ((f (g g)) x)))))
