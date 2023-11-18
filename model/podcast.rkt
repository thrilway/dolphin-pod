#lang racket/base
(require db
         racket/contract
         net/url
         "../workers.rkt"
         "structs.rkt"
         "author.rkt"
         "entity.rkt"
         "tag.rkt")
(define/contract (init-pod-podcasts! a-pod)
  (-> pod? void?)
  (unless (table-exists? (pod-db a-pod) "podcast_authors")
    (init-pod-podcast-authors! a-pod))
  (unless (table-exists? (pod-db a-pod) "entities")
    (init-pod-entities! a-pod))
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE podcasts ("
               "id bigint PRIMARY KEY REFERENCES entities (id) ON DELETE CASCADE, "
               "feed_url text NOT NULL UNIQUE, "
               "title text, "
               "author_id integer REFERENCES podcast_authors (id) ON DELETE CASCADE, "
               "explicit boolean, "
               "description text, "
               "image_url text)")))
(define/contract (pod-insert-podcast! a-pod feed-url)
  (-> pod? url? pod-podcast?)
  (let ((id-or-false (query-maybe-value (pod-db a-pod)
                                        "SELECT id FROM podcasts WHERE feed_url = $1"
                                        (url->string feed-url))))
    (if id-or-false
        (pod-podcast a-pod id-or-false)
        (with-handlers ((exn:fail? (lambda (ex) (raise-user-error 'pod-insert-podcast
                                                                  "ERROR processing ~a:\n~a\n"
                                                                  (url->string feed-url)
                                                                  (exn-message ex)))))
          (let ((podcast-info (get-podcast-info feed-url)))
            (if (string? podcast-info)
                (error podcast-info)
                (let* ((pid (query-value (pod-db a-pod)
                                         "INSERT INTO entities (type) VALUES ('podcast') RETURNING id"))
                       (author (if (hash-has-key? podcast-info 'author) (pod-insert-author! (hash-ref podcast-info 'author)) #f))
                       (the-podcast (pod-podcast a-pod pid)))
                  (query-exec (pod-db a-pod)
                              (string-append
                               "INSERT INTO podcasts "
                               "(id, feed_url, title, author_id, explicit, description, image_url) VALUES "
                               "($1, $2, $3, $4, $5, $6, $7)")
                            pid
                            (hash-ref podcast-info 'feed_url)
                            (hash-ref podcast-info 'title)
                            (if author (pod-author-id author) sql-null)
                            (hash-ref podcast-info 'explicit sql-null)
                            (hash-ref podcast-info 'description sql-null)
                            (hash-ref podcast-info 'image-url sql-null))
                (for ((tag (hash-ref podcast-info 'tags '())))
                  (let ((the-tag (pod-insert-tag! a-pod tag)))
                    (pod-insert-tag-entity-rel! a-pod
                                                the-tag
                                                the-podcast)))
                (for ((episode (hash-ref podcast-info 'episodes '())))
                  (if (string? episode)
                      (printf "\nfeed-url: ~a\n~a\n" (hash-ref podcast-info 'feed_url) episode)
                      (pod-insert-episode! a-pod the-podcast episode)))
                the-podcast)))))))
                    
(define/contract (pod-podcast-feed-url a-podcast)
  (-> pod-podcast? url?)
  (string->url
   (query-value (pod-db (pod-podcast-pod a-podcast))
                "SELECT feed_url FROM podcasts WHERE id = $1"
                (pod-podcast-id a-podcast))))
(define/contract (pod-podcast-title a-podcast)
  (-> pod-podcast? string?)
   (query-value (pod-db (pod-podcast-pod a-podcast))
                "SELECT title FROM podcasts WHERE id = $1"
                (pod-podcast-id a-podcast)))
(define/contract (pod-podcast-author a-podcast)
  (-> pod-podcast? pod-author?)
  (pod-author (pod-podcast-pod a-podcast)
              (query-value (pod-db (pod-podcast-pod a-podcast))
                "SELECT author_id FROM podcasts WHERE id = $1"
                (pod-podcast-id a-podcast))))
(define/contract (pod-podcast-explicit a-podcast)
  (-> pod-podcast? (or/c null? boolean?))
   (query-value (pod-db (pod-podcast-pod a-podcast))
                "SELECT explicit FROM podcasts WHERE id = $1"
                (pod-podcast-id a-podcast)))
(define/contract (pod-podcast-description a-podcast)
  (-> pod-podcast? string?)
   (query-value (pod-db (pod-podcast-pod a-podcast))
                "SELECT description FROM podcasts WHERE id = $1"
                (pod-podcast-id a-podcast)))
(define/contract (pod-podcast-image-url a-podcast)
  (-> pod-podcast? url?)
  (string->url
   (query-value (pod-db (pod-podcast-pod a-podcast))
                "SELECT image_url FROM podcasts WHERE id = $1"
                (pod-podcast-id a-podcast))))
(define/contract (pod-podcast-tags a-podcast)
  (-> pod-podcast? (listof pod-tag?))
  (define/contract (id->tag an-id)
    (-> integer? pod-tag?)
    (pod-tag (pod-podcast-pod a-podcast) an-id))
  (map id->tag
       (query-list (pod-db (pod-podcast-pod a-podcast))
                   "SELECT tag_id FROM tag_entity_rels WHERE podcast_id = $1"
                   (pod-podcast-id a-podcast))))


(provide (all-defined-out))