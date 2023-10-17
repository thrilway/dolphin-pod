#lang racket/base
(require db
         "workers.rkt")

(struct pod (db))
(struct pod-user (pod id))
(struct pod-podcast (pod id))

(define (init-pod! database user #:password (password #f) #:socket (socket 'guess))
  (define db (postgresql-connect
              #:user user
              #:database database
              #:socket 'guess))
  (define the-pod (pod db))
  (unless (table-exists? db "users")
    (query-exec db
                (string-append
                 "CREATE TABLE users "
                 "(id serial NOT NULL, username varchar(50) NOT NULL UNIQUE, "
                 "email text NOT NULL UNIQUE, password text NOT NULL, "
                 "icon bytea, name text, "
                 "manually_approves_follows boolean DEFAULT TRUE, "
                 "date_created timestamp (0) DEFAULT CURRENT_DATE, "
                 "CONSTRAINT user_key PRIMARY KEY (id)"
                 ")"))
    (pod-insert-user! the-pod
                              "jhill"
                              "joe.hill@iww.ca"
                              "APassword1")
    (pod-insert-user! the-pod
                              "emma.goldman"
                              "e@riseup.ca"
                              "Anarchy4Eva")
    (pod-update-user! (pod-user the-pod 2) 'manually_approves_follows #f))
  (unless (table-exists? db "podcasts")
    (query-exec db
                (string-append
                 "CREATE TABLE podcasts "
                 "(id serial NOT NULL, feed_url text NOT NULL UNIQUE, title text, description text, cover_art bytea, "
                 "CONSTRAINT podcast_key PRIMARY KEY (id))"))
    (query-exec db
                (string-append
                 "CREATE TABLE episodes "
                 "(id serial NOT NULL, "
                 "file_url text NOT NULL, "
                 "title text NOT NULL, "
                 "description text, "
                 "date_published timestamp, "
                 "duration integer, "
                 "mime_type text, "
                 "podcast_id integer REFERENCES podcasts (id))"))
    (pod-insert-podcast! the-pod
                                 "https://feeds.acast.com/public/shows/thesloppyboys")
    (pod-insert-podcast! the-pod
                                 "https://feeds.redcircle.com/c77fa711-6942-4cc5-a864-93521650ac95")
    (pod-insert-podcast! the-pod
                                 "https://itsgoingdown.org/category/podcast/feed"))
  (unless (table-exists? db "subscriptions")
    (query-exec db
                (string-append
                 "CREATE TABLE subscriptions "
                 "(id serial PRIMARY KEY, "
                 "user_id integer REFERENCES users (id) ON DELETE CASCADE, "
                 "podcast_id integer REFERENCES podcasts (id) ON DELETE CASCADE, "
                 "subscription_date timestamp (0) DEFAULT CURRENT_DATE"
                 ")"))
    (pod-insert-subscription! the-pod
                                      (pod-user the-pod 1)
                                      (pod-podcast the-pod 1))
    (pod-insert-subscription! the-pod
                                      (pod-user the-pod 1)
                                      (pod-podcast the-pod 2))
    (pod-insert-subscription! the-pod
                                      (pod-user the-pod 2)
                                      (pod-podcast the-pod 1))
    (pod-insert-subscription! the-pod
                                      (pod-user the-pod 2)
                                      (pod-podcast the-pod 3)))
  (unless (table-exists? db "follows")
    (query-exec db
                (string-append
                 "CREATE TABLE follows ("
                 "id serial PRIMARY KEY, "
                 "follower_id integer NOT NULL REFERENCES users (id) ON DELETE CASCADE, "
                 "followed_id integer NOT NULL REFERENCES users (id) ON DELETE CASCADE, "
                 "follow_date timestamp (0) DEFAULT CURRENT_DATE, "
                 "follow_accepted BOOLEAN"
                 ")"))
    (pod-insert-follow! the-pod
                        (pod-user the-pod 1)
                        (pod-user the-pod 2))
    (pod-insert-follow! the-pod
                        (pod-user the-pod 2)
                        (pod-user the-pod 1)))
  the-pod)

(define (pod-users a-pod)
  (define (id->user id)
    (pod-user a-pod id))
  (map id->user
       (query-list (pod-db a-pod)
                   "SELECT id FROM users")))

(define (pod-insert-user! a-pod username email password)
  (query-exec (pod-db a-pod)
              (string-append
               "INSERT INTO users "
               "(username, email, password) "
               "VALUES "
               "($1, $2, crypt($3, gen_salt('bf')))")
              username email password))
(define (pod-update-user! a-user key value)
  (query-exec (pod-db (pod-user-pod a-user))
              (string-append
               "UPDATE users "
               "SET $2 = $3 "
               "WHERE users.id = $1")
              (pod-user-id a-user)
              key
              value))

(define (pod-user-manually-approves-follows? a-user)
  (query-value (pod-db (pod-user-pod a-user))
               "SELECT manually_approves_follows FROM users WHERE id = $1"
               (pod-user-id a-user)))
               
(define (pod-insert-podcast! a-pod feed_url)
  (let-values (((cast-info eps) (get-podcast-info feed_url)))
    (let ((the-podcast
           (pod-podcast
            (pod-db a-pod)
            (query-value (pod-db a-pod)
                        (string-append
                         "INSERT INTO podcasts "
                         "(feed_url, title, description, cover_art) "
                         "VALUES "
                         "($1, $2, $3, $4) "
                         "RETURNING id")
                        feed_url
                        (hash-ref cast-info 'title)
                        (hash-ref cast-info 'description)
                        (hash-ref cast-info 'cover_art)))))
      (insert-pod-episodes! the-podcast eps))))

(define (insert-pod-episode! a-podcast ep)
  (query-exec (pod-db (pod-podcast-pod a-podcast))
              (string-append
               "INSERT INTO episodes "
               "(file_url, title, description, date_published, duration, mime_type, podcast_id) "
               "VALUES ($1, $2, $3, $4, $5, $6, $7)")
              (hash-ref ep 'file_url)
              (hash-ref ep 'title)
              (hash-ref ep 'description)
              (hash-ref ep 'date_published)
              (hash-ref ep 'duration)
              (hash-ref ep 'mime_type)
              (pod-podcast-id a-podcast)))
(define (insert-pod-episodes! a-podcast eps)
  (for ((ep eps))
    (insert-pod-episode! a-podcast ep)))
               

(define (pod-insert-subscription! a-pod a-user a-podcast)
  (query-exec (pod-db a-pod)
              (string-append
               "INSERT INTO subscriptions "
               "(user_id, podcast_id) "
               "VALUES "
               "($1, $2)")
              (pod-user-id a-user)
              (pod-podcast-id a-podcast)))

(define (pod-user-subscriptions a-pod a-user)
  (define (id->podcast id)
    (pod-podcast a-pod id))
  (map id->podcast
       (query-list (pod-db a-pod)
                   "SELECT podcast_id FROM subscriptions WHERE user_id=$1"
                   (pod-user-id a-user))))

(define (pod-podcast-subscribers a-pod a-podcast)
  (define (id->user id)
    (pod-user a-pod id))
  (map id->user
       (query-list (pod-db a-pod)
                   "SELECT user_id FROM subscriptions WHERE podcast_id=$1"
                   (pod-podcast-id a-podcast))))

(define (pod-insert-follow! a-pod a-user1 a-user2)
  (query-exec (pod-db a-pod)
              (string-append
               "INSERT INTO follows "
               "(follower_id, followed_id, follow_accepted) "
               "VALUES ($1, $2, $3)")
              (pod-user-id a-user1)
              (pod-user-id a-user2)
              (if (pod-user-manually-approves-follows? a-user2)
                  #f
                  #t)))