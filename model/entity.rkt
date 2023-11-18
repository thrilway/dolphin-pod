#lang racket/base
(require db
         racket/contract
         racket/match
         "structs.rkt")
(define/contract (init-pod-entity-type! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TYPE entity_type AS ENUM ("
               "'podcast', "
               "'episode', "
               "'review', "
               "'comment')")))
(define/contract (init-pod-entities! a-pod)
  (-> pod? void?)
  (unless (query-maybe-value (pod-db a-pod)
                             "SELECT TRUE FROM pg_type WHERE typname = $1"
                             "entity_type")
    (init-pod-entity-type! a-pod))
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE entities ("
               "id bigserial PRIMARY KEY, "
               "type entity_type NOT NULL)")))
(define/contract (pod-entity? v)
  (-> any/c boolean?)
  (for/or ((pred? (list pod-podcast?
                        pod-episode?
                        pod-review?
                        pod-comment?)))
    (pred? v)))
(define/contract (pod-entity-id v)
  (-> pod-entity? integer?)
  (match v
    ((struct pod-podcast (pod id)) id)
    ((struct pod-episode (pod id)) id)
    ((struct pod-review (pod id)) id)
    ((struct pod-comment (pod id)) id)))
(define/contract (pod-get-entity a-pod an-id)
  (-> pod? integer? pod-entity?)
  (define entity-structs
    (hash "podcast" pod-podcast
          "episode" pod-episode
          "review" pod-review
          "comment" pod-comment))
  (let ((type (query-value (pod-db a-pod)
                           "SELECT type FROM entities WHERE id = $1"
                           an-id)))
    ((hash-ref entity-structs type) a-pod an-id)))
(provide (all-defined-out))