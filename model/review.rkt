#lang racket/base
(require
  racket/contract
  db
  "structs.rkt")
(define/contract (init-pod-reviews! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE podcast_reviews ("
               "id bigint PRIMARY KEY REFERENCES entities (id) ON DELETE CASCADE, "
               "podcast_id bigint NOT NULL REFERENCES podcasts (id) ON DELETE CASCADE, "
               "profile_id integer NOT NULL REFERENCES profiles (id) ON DELETE CASCADE, "
               "review text, rating smallint, "
               "recommended_episode_id bigint REFERENCES episodes (id) ON DELETE CASCADE, "
               "created_on timestamp with time zone DEFAULT CURRENT_TIMESTAMP, "
               "CONSTRAINT rating_limit CHECK ( rating <= 5 AND rating > 0 ), "
               "CONSTRAINT podcast_profile_unique UNIQUE (podcast_id, profile_id))")))
(define/contract (pod-insert-review! a-profile a-podcast #:review-text (review-text #f) #:rating (rating #f) #:episode (recommended-ep #f))
  (->* (pod-profile? pod-podcast?) (#:review-text (or/c string? #f) #:rating (or/c integer? #f) #:episode (or/c pod-episode? #f)) (or/c pod-review? #f))
  (define review-id
    (or
     (query-maybe-value (pod-db (pod-profile-pod a-profile))
                        (string-append
                         "SELECT id FROM reviews "
                         "WHERE podcast_id = $1 "
                         "AND profile_id = $2")
                        (pod-podcast-id a-podcast)
                        (pod-profile-id a-profile))
     (query-value (pod-db (pod-profile-pod a-profile))
                  (string-append
                   "INSERT INTO entities (type) VALUES ('review') "
                   "RETURNING id"))))
  (begin
    (query-exec (pod-db (pod-profile-pod a-profile))
                (string-append
                 "INSERT INTO reviews VALUES"
                 "($1, $2, $3, $4, $5, $6) "
                 "ON CONFLICT ON CONSTRAINT podcast_profile_unique "
                 "DO UPDATE SET (review, rating, recommended_episode_id) = ($4, $5, $6)")
                review-id
                (pod-podcast-id a-podcast)
                (pod-profile-id a-profile)
                (false->sql-null review-text)
                (false->sql-null rating)
                (if recommended-ep
                    (pod-episode-id recommended-ep)
                    sql-null))
    (pod-review (pod-profile-pod a-profile) review-id)))