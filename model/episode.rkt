#lang racket/base
(require
  db
  racket/contract
  "structs.rkt"
  "tag.rkt"
  "rels.rkt"
  )
(define/contract (init-pod-episodes! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE episodes ("
               "id bigint PRIMARY KEY REFERENCES entities (id) ON DELETE CASCADE, "
               "podcast_id integer NOT NULL REFERENCES podcasts (id) ON DELETE CASCADE, "
               "file_url text NOT NULL UNIQUE, "
               "file_mimetype text NOT NULL, "
               "title text, "
               "description text, "
               "duration integer, "
               "image_url text, "
               "image_mimetype text, "
               "date_posted timestamp with time zone)")))
(define/contract (pod-insert-episode! a-pod a-podcast episode-info)
  (->* (pod? pod-podcast? hash?) pod-episode?)
  (let ((the-ep (pod-episode a-pod (query-value (pod-db a-pod) "INSERT INTO entities (type) VALUES ('episode') RETURNING id"))))
    (with-handlers ((exn:fail:sql?
                     (lambda (ex) (let ((msg (cdr (assoc 'message (exn:fail:sql-info ex)))))
                                    (raise-user-error 'pod-insert-episode "Episode: ~a\n~a\nSQLSTATE: ~a\n"
                                                      (hash-ref episode-info 'title)
                                                      msg (exn:fail:sql-sqlstate ex))))))
      (query-exec (pod-db a-pod)
                  (string-append
                   "INSERT INTO episodes VALUES "
                   "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)")
                  (pod-episode-id the-ep)
                  (pod-podcast-id a-podcast)
                  (hash-ref episode-info 'url)
                  (hash-ref episode-info 'file_mimetype)
                  (hash-ref episode-info 'title sql-null)
                  (hash-ref episode-info 'description sql-null)
                  (hash-ref episode-info 'duration sql-null)
                  (hash-ref episode-info 'image_url sql-null)
                  (hash-ref episode-info 'image_mimetype sql-null)
                  (hash-ref episode-info 'date_published sql-null)))
    (for ((tag (hash-ref episode-info 'tags '())))
      (let ((tag (pod-insert-tag! a-pod tag)))
        (pod-insert-tag-entity-rel! a-pod tag the-ep)))
    the-ep))
(define/contract (pod-episode-url an-episode)
  (-> pod-episode? string?)
  (query-value (pod-db (pod-podcast-pod an-episode))
               "SELECT file_url FROM episodes WHERE id = $1"
               (pod-episode-id an-episode)))
(define/contract (pod-episode-file-mimetype an-episode)
  (-> pod-episode? string?)
  (query-value (pod-db (pod-podcast-pod an-episode))
               "SELECT file_mimetype FROM episodes WHERE id = $1"
               (pod-episode-id an-episode)))
(define/contract (pod-episode-title an-episode)
  (-> pod-episode? string?)
  (query-value (pod-db (pod-podcast-pod an-episode))
               "SELECT title FROM episodes WHERE id = $1"
               (pod-episode-id an-episode)))
(define/contract (pod-episode-description an-episode)
  (-> pod-episode? (or/c string? #f))
  (query-maybe-value (pod-db (pod-podcast-pod an-episode))
                     "SELECT description FROM episodes WHERE id = $1"
                     (pod-episode-id an-episode)))
(define/contract (pod-episode-image-url an-episode)
  (-> pod-episode? (or/c string? #f))
  (or
   (query-maybe-value (pod-db (pod-podcast-pod an-episode))
                      "SELECT image_url FROM episodes WHERE id = $1"
                      (pod-episode-id an-episode))
   (let ((podcast-id (query-value (pod-db (pod-podcast-pod an-episode))
                                  "SELECT podcast_id FROM episodes WHERE id = $1"
                                  (pod-episode-id an-episode))))
     (query-maybe-value (pod-db (pod-podcast-pod an-episode))
                        "SELECT image_url FROM podcasts WHERE id = $1"
                        podcast-id))))
(define/contract (pod-episode-image-mimetype an-episode)
  (-> pod-episode? (or/c string? #f))
  (or
   (query-maybe-value (pod-db (pod-podcast-pod an-episode))
                      "SELECT image_mimetype FROM episodes WHERE id = $1"
                      (pod-episode-id an-episode))
   (let ((podcast-id (query-value (pod-db (pod-podcast-pod an-episode))
                                  "SELECT podcast_id FROM episodes WHERE id = $1"
                                  (pod-episode-id an-episode))))
     (query-maybe-value (pod-db (pod-podcast-pod an-episode))
                        "SELECT image_mimetype FROM podcasts WHERE id = $1"
                        podcast-id))))
(define/contract (pod-episode-date-published an-episode)
  (-> pod-episode? integer?)
  (let ((the-date
         (sql-datetime->srfi-date (query-value (pod-db (pod-podcast-pod an-episode))
                                               "SELECT date_posted FROM episodes WHERE id = $1"
                                               (pod-episode-id an-episode)))))
    (date->seconds the-date #f)))