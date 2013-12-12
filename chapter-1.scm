#lang racket

(require rackunit)

;;; 1.2 Deriving Recursive Programs

;; 1.2.5 subst

; subst : Sym x Sym x S-list -> S-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))

; subst-in-s-exp : Sym x Sym x S-exp -> S-exp
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))

(check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))

; Exercise 1.12
(define subst-inline
  (lambda (new old slist)
    (if (null? slist)
        '()    
        (cons
         (if (symbol? (car slist))
             (if (eqv? (car slist) old) new (car slist))
             (subst-inline new old (car slist)))
         (subst-inline new old (cdr slist))))))

(check-equal? (subst-inline 'a 'b '((b c) (b () d))) '((a c) (a () d)))

; Exercise 1.13
(define subst-map
  (lambda (new old slist)
    (map (lambda (sexp)
           (if (symbol? sexp)
               (if (eqv? sexp old) new sexp)
               (subst-map new old sexp)))
         slist)))

(check-equal? (subst-map 'a 'b '((b c) (b () d))) '((a c) (a () d)))

;;; 1.3 Auxiliary Procedures and Context Arguments

; number-of-elements-from : Listof(SchemeVal) x Int -> Listof(List(Int, SchemeVal))
(define number-elements-from
  (lambda (lst n)
    (if (null? lst) '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))

(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

(check-equal? (number-elements '(a)) '((0 a)))
(check-equal? (number-elements '(a b c d e f g h i))
              '((0 a) (1 b) (2 c) (3 d) (4 e) (5 f) (6 g) (7 h) (8 i)))

;;; 1.4 Exercises

; Exercise 1.15
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

(check-equal? (duple 2 3) '(3 3))
(check-equal? (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
(check-equal? (duple 0 '(blah)) '())

; Exercise 1.16
(define invert
  (lambda (lst)
    (map (lambda (pair)
           (list (cadr pair) (car pair)))
         lst)))

(check-equal? (invert '((a 1) (a 2) (1 b) (2 b)))
              '((1 a) (2 a) (b 1) (b 2)))

; Exercise 1.17
(define down
  (lambda (lst)
    (map list lst)))

(check-equal? (down '(1 2 3)) '((1) (2) (3)))
(check-equal? (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
(check-equal? (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))

; Exercise 1.18
(define swapper
  (lambda (s1 s2 slist)
    (map (lambda (sexp)
           (if (symbol? sexp)
               (cond
                 ((eqv? s1 sexp) s2)
                 ((eqv? s2 sexp) s1)
                 (else sexp))
               (swapper s1 s2 sexp)))
         slist)))

(check-equal? (swapper 'a 'd '(a b c d)) '(d b c a))
(check-equal? (swapper 'a 'd '(a d () c d)) '(d a () c a))
(check-equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))

; Exercise 1.19
(define list-set
  (lambda (lst n x)
    (if (null? lst)
        (error "list too short")
        (if (zero? n)
            (cons x (cdr lst))
            (cons (car lst) 
                  (list-set (cdr lst) (- n 1) x))))))

(check-equal? (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10))

; Exercise 1.34

(define path-with-directions
  (lambda (n bst dirs)
    (cond
      ((null? bst) '())
      ((< n (car bst))
       (path-with-directions
        n (cadr bst) (append dirs '(left))))
      ((> n (car bst))
       (path-with-directions
        n (caddr bst) (append dirs '(right))))
      (else dirs))))

(define path
  (lambda (n bst)
    (path-with-directions n bst '())))

(check-equal? (path 17 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ()))))
              '(right left left))
(check-equal? (path 100 '()) '())
(check-equal? (path 13 '(5 () ())) '())
(check-equal? (path 7 '(2 () (7 () ()))) '(right))
