#lang racket

(require web-server/http/bindings
         web-server/http/json
         "model.rkt")

(provide webfinger)

(define (webfinger req)
  (define bindings (request-bindings req))
  (define host (cdr (assq 'host (request-headers req))))
  (response/jsexpr (get-webfinger bindings host)))
