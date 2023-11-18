#lang racket/base
(require db
         racket/contract
         "structs.rkt")

(define/contract (init-pod-podcast-authors! a-pod)
  (-> pod? void)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE podcast_authors ("
               "id serial PRIMARY KEY, "
               "name text NOT NULL UNIQUE, "
               "email text)")))
(define/contract (pod-insert-author! a-pod name)
  (-> pod? string? pod-author?)
  (pod-author a-pod
              (query-value (pod-db a-pod)
                           (string-append
                            "INSERT INTO authors (name) VALUES ($1) "
                            "RETURNING id")
                            name)))
(define/contract (pod-author-name an-author)
  (-> pod-author? string?)
  (query-value (pod-db (pod-author an-author))
                "SELECT name FROM podcast_authors WHERE id = $1"
                (pod-author-id an-author)))
(define/contract (pod-author-podcasts an-author)
  (-> pod-author? (listof pod-podcast?))
  (map (lambda (id) (pod-podcast (pod-author-pod an-author) id))
       (query-list (pod-db (pod-author-pod an-author))
                   (string-append
                    "SELECT id FROM podcasts "
                    "WHERE author_id = $1")
                   (pod-author-id an-author))))

(provide (all-defined-out))