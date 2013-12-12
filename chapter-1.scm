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

; Exercise 1.13
(define subst-map
  (lambda (new old slist)
    (map (lambda (sexp)
           (if (symbol? sexp)
               (if (eqv? sexp old) new sexp)
               (subst-map new old sexp)))
         slist)))

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

;;; 1.4 Exercises

; Exercise 1.15
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

; Exercise 1.16
(define invert
  (lambda (lst)
    (map (lambda (pair)
           (list (cadr pair) (car pair)))
         lst)))

; Exercise 1.17
(define down
  (lambda (lst)
    (map list lst)))

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

; Exercise 1.19
(define list-set
  (lambda (lst n x)
    (if (null? lst)
        (error "list too short")
        (if (zero? n)
            (cons x (cdr lst))
            (cons (car lst) 
                  (list-set (cdr lst) (- n 1) x))))))

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

;;; Tests

(subst 'a 'b '((b c) (b () d)))
(subst-inline 'a 'b '((b c) (b () d)))
(subst-map 'a 'b '((b c) (b () d)))

(number-elements '(a b c d e f g h i))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))

(invert '((a 1) (a 2) (1 b) (2 b)))

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))

(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)

(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))
(path 100 '())
(path 13 '(5 () ()))
(path 7 '(2 () (7 () ())))
