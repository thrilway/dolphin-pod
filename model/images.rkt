#lang racket/base
(require
  racket/contract
  db
  "structs.rkt"
         )
(define/contract (init-pod-images! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE images ("
               "id serial PRIMARY KEY, "
               "image bytea NOT NULL, "
               "mime_type varchar(50) NOT NULL)")))

(provide (all-defined-out))