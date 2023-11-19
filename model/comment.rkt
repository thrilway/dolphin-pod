#lang racket/base
(require racket/contract
         db
         "structs.rkt")
(define/contract (init-pod-comments! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE comments ("
               "id bigint PRIMARY KEY REFERENCES entities (id) ON DELETE CASCADE, "
               "review_id bigint NOT NULL REFERENCES podcast_reviews (id) ON DELETE CASCADE, "
               "profile_id integer NOT NULL REFERENCES profiles (id) ON DELETE CASCADE, "
               "in_reply_to_id bigint REFERENCES comments (id) ON DELETE CASCADE, "
               "comment_text text NOT NULL, "
               "created_on timestamp with time zone DEFAULT CURRENT_TIMESTAMP)")))