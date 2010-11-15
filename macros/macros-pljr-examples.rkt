#lang racket

(require test-engine/racket-tests)
(require (for-syntax syntax/parse))


;; ----------------------------------------------------------------------------
;; my-cond



(define-syntax my-cond1
  (syntax-rules (else)
    [(_ [else body]) body]
    [(_ [guard body])
     (let ([use-this-clause? guard])
       (if use-this-clause?
           body
           #f))]
    [(_ [guard body] clause ...)
     (let ([use-this-clause? guard])
       (if use-this-clause?
           body
           (my-cond1 clause ...)))]))


(define-syntax my-cond2
  (syntax-rules (else =>)
    [(_ [else body]) body]
    [(_ [guard body])
     (let ([use-this-clause? guard])
       (if use-this-clause?
           body
           #f))]
    [(_ [test-expr => fn])
     (let ([test-expr-maybe test-expr])
       (if test-expr-maybe
           (fn test-expr-maybe)
           #f))]
    [(_ [test-expr => fn] clause ...)
     (let ([test-expr-maybe test-expr])
       (if test-expr-maybe
           (fn test-expr-maybe)
           (my-cond2 clause ...)))]
    [(_ [guard body] clause ...)
     (let ([use-this-clause? guard])
       (if use-this-clause?
           body
           (my-cond2 clause ...)))]))

(check-expect (my-cond1 [#t 1]) 1)
(check-expect (my-cond1 [#f 2]) #f)
(check-expect (my-cond1 [(= 1 2) 1] [(= 2 2) (+ 4 5)]) 9)
(check-expect (my-cond1 [else (+ 3 4)]) 7)
(check-expect (my-cond1 [(< 1 0) (+ 4 5)] [else (+ 4 5)]) 9)
; (my-cond (my-cond1 [(#f 1)] [else 2] [#f 3])) -> error
(check-expect (my-cond2 [1 => (λ (x) (add1 x))]) 2)
(check-expect (my-cond2 [#f => (λ (x) x)] [2 => (λ (x) (+ x 40))]) 42)


;; ----------------------------------------------------------------------------
;; my-let

(define-syntax my-let
  (syntax-rules ()
    [(_ ([x val] ...) body)
     ((λ (x ...) body) val ...)]
    [(_ name ([x val] ...) body)
     (letrec ([name (λ (x ...) body)]) 
       (name val ...))]))

(define-syntax (my-let-checked1 stx)
  (syntax-case stx ()
    [(_ ([x val] ...) body)
     (and (andmap identifier? (syntax->list #'(x ...)))
          (not (check-duplicate-identifier #'(x ...))))
     #'((λ (x ...) body) val ...)]))

(define-syntax (my-let-checked2 stx)
  (syntax-case stx ()
    [(_ ([x val] ...) body)
     (begin
       (for-each 
        (λ (x)
          (unless (identifier? x)
            (raise-syntax-error
             'not-identifier "expected identifier" stx x)))
        (syntax->list #'(x ...)))
       (let ([dup (check-duplicate-identifier #'(x ...))])
         (when dup
           (raise-syntax-error 
            'duplicate-var "duplicate variable name" stx dup))))
     #'((λ (x ...) body) val ...)]))

(define-syntax (my-let-checked3 stx)
  (syntax-parse 
   stx 
   [(_ ([x:identifier val:expr] ...) body:expr)
    #:fail-when (check-duplicate-identifier #'(x ...))
    "duplicate variable name"
    #'((λ (x ...) body) val ...)]))




(check-expect
 (my-let ([x 1] 
          [y (+ 2 3)])
         (+ 4 x y))
 10)
(check-expect
 (my-let 
  loop ([x 5]) 
  (if (= x 0) 
      0 
      (+ x (loop (sub1 x)))))
 15)
; (my-let ([1 2]) 3) ; bad syntax, var should be identifier, get λ error
; (my-let ([x 1) [x 2]) x) ; bad syntax, duplicate var, get λ error
; (my-let ([(x y) 4]) x) ; bad syntax, thinks lambda arg has optional param
; (my-let ([x (+ 1)]) x) ; bad syntax, rhs
; (my-let ([x 1 2]) x) ; bad syntax, rhs

;; ----------------------------------------------------------------------------
;; my-time

(define-syntax my-time
  (syntax-rules ()
    [(_ expr)
     (let ([start-time (current-milliseconds)])
       (begin
         expr
         (- (current-milliseconds) start-time)))]))




;; ----------------------------------------------------------------------------
;; loop with break

(define-syntax (loop stx)
  (syntax-case stx ()
    [(_ body)
     (with-syntax
         ([exit-id (datum->syntax stx 'exit)])
       #'(call/cc
          (λ (exit-id)
            (letrec ([f (λ () (begin body (f)))]) 
              (f)))))]))
;; ----------------------------------------------------------------------------
;; cas-cad-e

(define-syntax (cas-cad-e stx)
 (syntax-case stx ()
   [(_ e ((v) exp ...))
    (with-syntax ([break (datum->syntax stx 'break)])
      #'(call/cc
         (λ (break)
           (if (equal? e v)
               (begin exp ...)
               (void)))))]
   [(_ e ((v1) exp1 ...) ((v2) exp2 ...) ...)
    (with-syntax ([break (datum->syntax stx 'break)])
      #'(call/cc
         (λ (break)
           (if (equal? e v1)
               (begin exp1 ... exp2 ... ...)
               (cas-cad-e e ((v2) exp2 ...) ...)))))]))

(define (cas1 v)
  (cas-cad-e v
           ((1) (display "1"))
           ((2) (display "2") (break 2)
           ((3) 3))))

;(cas1 1) ==> 2       (and prints "12")
;(cas1 2) ==> 2       (and prints "2")
;(cas1 3) ==> 3       (and prints nothing)
;(cas1 4) ==> <void>  (and prints nothing)

(define (cas3 v)
 (let ([w true])
   (cas-cad-e 
    v
    [(0) (set! w false)]
    [(1) (if w (break 1) (break 0))])))

(check-expect (cas3 0) 0)
(check-expect (cas3 1) 1)

(test)