#lang racket/base
(require db
        racket/contract
        "structs.rkt")
(define/contract (init-pod-tags! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE tags ("
               "id serial PRIMARY KEY, "
               "name text NOT NULL)")))
(define/contract (pod-insert-tag! a-pod tag)
  (-> pod? string? pod-tag?)
  (pod-tag a-pod
           (query-value (pod-db a-pod)
                        "INSERT INTO tags (name) VALUES ($1) RETURNING id"
                        tag)))
(provide (all-defined-out))