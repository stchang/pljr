#lang racket

(require test-engine/racket-tests)


;; ----------------------------------------------------------------------------
;; my-cond

(define-syntax my-cond
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
           (my-cond clause ...)))]
    [(_ [guard body] clause ...)
     (let ([use-this-clause? guard])
       (if use-this-clause?
           body
           (my-cond clause ...)))]))

(check-expect (my-cond [#t 1]) 1)
(check-expect (my-cond [#f 2]) #f)
(check-expect (my-cond [(= 1 2) 1] [(= 2 2) (+ 4 5)]) 9)
(check-expect (my-cond [else (+ 3 4)]) 7)
(check-expect (my-cond [(< 1 0) (+ 4 5)] [else (+ 4 5)]) 9)
; (my-cond (my-cond [(#f 1)] [else 2] [#f 3])) -> error
(check-expect (my-cond [1 => (λ (x) (add1 x))]) 2)
(check-expect (my-cond [#f => (λ (x) x)] [2 => (λ (x) (+ x 40))]) 42)


;; ----------------------------------------------------------------------------
;; my-let

(define-syntax my-let
  (syntax-rules ()
    [(_ ([x val] ...) body)
     ((λ (x ...) body) val ...)]
    [(_ name ([x val] ...) body)
     (letrec ([name (λ (x ...) body)]) 
       (name val ...))]))

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