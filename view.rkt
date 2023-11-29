#lang racket

(define (is_api_req? req)
  (let ((accept (cdr (assq 'accept (get-headers req)))))
    (regexp-match? #rx".*json.*" accept)))