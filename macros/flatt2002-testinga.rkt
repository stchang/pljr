#lang racket

;(provide test-val)
;(define test-val 23)

(provide (for-syntax test-val))
(define-for-syntax test-val 23)