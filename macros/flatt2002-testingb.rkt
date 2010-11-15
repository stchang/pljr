#lang racket
(require (for-syntax "testinga.rkt"))

#;(define-syntax (tester stx)
  (syntax-case stx ()
    [_ #`#,test-val]))

;(require (for-meta 2 racket/base))
#;(define-syntax (tester stx)
  (define-syntax (tester2 stx)
    (syntax-case stx ()
      [_ #`#,test-val]))
  #`#,(tester2))