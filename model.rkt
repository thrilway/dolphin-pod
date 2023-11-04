#lang racket/base
(require db
         "workers.rkt"
         racket/match
         dotenv)

(struct pod (db))
(struct pod-user (pod id))
(struct pod-podcast (pod id))
(struct pod-episode (pod id))

(define (getenv-or-die var)
  (unless (string? var)
    (error "Variable should be a string:" var))
  (define val (getenv var))
  (unless (string? val)
    (error (format "Mandatory environment variable \"~a\" is unset." var)))
  val)
(define (getenv-or-false var)
  (unless (string? var)
    (error "Variable should be a string:" var))
  (let ((val (getenv var)))
    (if (string? val)
        val
        #f)))

(define (load-db)
  (dotenv-load! (list ".env"))
  (define host (getenv-or-false "DB_HOST"))
  (define port (getenv-or-false "DB_PORT"))
  (define user (getenv-or-false "DB_USER"))
  (define pass (getenv-or-false "DB_PASSWORD"))
  (define database (getenv-or-false "DB_DATABASE"))
  (define port/number (if (string? port)
			(string->number port
					10
					'number-or-false)
			#f))
  (if port
    (postgresql-data-source
      #:user user
      #:port port/number
      #:server host
      #:password pass 
      #:database database)
    (postgresql-data-source
      #:user user
      #:database database
      #:socket 'guess)))

(define db-source (load-db))
(define (connect!)
  (dsn-connect db-source))

(define (init-pod!)
  (define db
    (virtual-connection
     (connection-pool connect!)))
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
    (pod-update-user! (pod-user the-pod 2) (list (cons 'manually_approves_follows #f))))
  (unless (table-exists? db "podcasts")
    (query-exec db
                (string-append
                 "CREATE TABLE podcasts "
                 "(id serial NOT NULL, feed_url text NOT NULL UNIQUE, title text, description text, cover_art bytea, "
                 "CONSTRAINT podcast_key PRIMARY KEY (id))"))
     (unless (table-exists? db "episodes")
       (query-exec db
                   (string-append
                    "CREATE TABLE episodes "
                    "(id serial PRIMARY KEY, "
                    "file_url text NOT NULL, "
                    "title text NOT NULL, "
                    "description text, "
                    "date_published timestamp, "
                    "podcast_id integer REFERENCES podcasts (id))")))
    (unless (table-exists? db "tags")
      (query-exec db
                  (string-append
                   "CREATE TABLE tags "
                   "(id serial PRIMARY KEY, "
                   "tag_term text NOT NULL)"
                   )))
    (unless (table-exists? db "podcast_tags")
      (query-exec db
                  (string-append
                   "CREATE TABLE podcast_tags "
                   "(id serial PRIMARY KEY, "
                   "podcast_id integer REFERENCES podcasts (id), "
                   "tag_id integer REFERENCES tags (id))")))
    (unless (table-exists? db "episode_tags")
      (query-exec db
                  (string-append
                   "CREATE TABLE episode_tags "
                   "(id serial PRIMARY KEY, "
                   "episode_id integer REFERENCES episodes (id), "
                   "tag_id integer REFERENCES tags (id))")))
    
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
  (unless (table-exists? db "users_episodes_rels")
    (query-exec db
                (string-append
                 "CREATE TABLE users_episodes_rels ("
                 "id serial PRIMARY KEY, "
                 "user_id integer NOT NULL REFERENCES users (id) ON DELETE CASCADE, "
                 "episode_id integer NOT NULL REFERENCES episodes (id) ON DELETE CASCADE, "
                 "seconds_listented integer DEFAULT 0, "
                 "episode_played BOOLEAN DEFAULT FALSE)")))
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
  (unless (table-exists? db "user_podcast_reviews")
    (query-exec db
                (string-append
                 "CREATE TABLE user_podcast_reviews ("
                 "id serial PRIMARY KEY, "
                 "user_id integer REFERENCES users (id) ON DELETE CASCADE, "
                 "podcast_id integer REFERENCES podcasts (id) ON DELETE CASCADE, "
                 "rating smallint, review text)")))
  the-pod)

(define (clear-pod! database user #:password (password #f) #:socket (socket 'guess))
    (define db (postgresql-connect
              #:user user
              #:database database
              #:socket 'guess))
  (query-exec db
              (string-append
               "DROP TABLE IF EXISTS "
               "users, "
               "podcasts, "
               "episodes "
               "CASCADE")))

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
(define (pod-update-user! a-user bindings)
  (for ((kv bindings))
    (match kv
      ((cons 'icon v) (pod-update-user-icon! a-user v))
      ((cons 'manually_approves_follows v) (pod-update-user-maf! a-user v))
      (else (void)))))
  
(define (pod-update-user-icon! a-user img)
  (query-exec (pod-db (pod-user-pod a-user))
              (string-append
               "UPDATE users "
               "SET icon = $2 "
               "WHERE users.id = $1")
              (pod-user-id a-user)
              img))
(define (pod-update-user-maf! a-user maf?)
  (query-exec (pod-db (pod-user-pod a-user))
              (string-append
               "UPDATE users "
               "SET manually_approves_follows = $2 "
               "WHERE users.id = $1")
              (pod-user-id a-user)
              maf?))

(define (pod-user-manually-approves-follows? a-user)
  (query-value (pod-db (pod-user-pod a-user))
               "SELECT manually_approves_follows FROM users WHERE id = $1"
               (pod-user-id a-user)))
(define (pod-select-user a-pod username)
  (let ((res (query (pod-db a-pod)
                    "SELECT * FROM users WHERE username = $1"
                    username)))
    (for/fold ((out (hash)) #:result out)
              ((head (rows-result-headers res))
               (val (in-vector (car (rows-result-rows res)))))
      (if (string=? (cdr (assv 'name head)) "password")
          out
          (hash-set out (string->symbol (cdr (assv 'name head))) val)))))
               
(define (pod-insert-podcast! a-pod feed_url (a-user '()))
  (let ((cast-info (get-podcast-info feed_url)))
    (let ((the-podcast
           (pod-podcast
            a-pod
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
      (when a-user
        (pod-insert-subscription! a-pod a-user the-podcast))
      (thread (insert-podcast-tags! the-podcast (hash-ref cast-info 'tags)))
      (thread (insert-pod-episodes! the-podcast (hash-ref cast-info 'episodes))))))

(define (insert-podcast-tags! a-podcast tags)
  (let loop ((rem tags))
    (if (null? rem)
        (void)
        (begin
          (let ((tag_id (or
                         (query-maybe-value
                          (pod-db (pod-podcast-pod a-podcast))
                          "SELECT id FROM tags WHERE tag_term = $1"
                          (car rem))
                         (query-value
                          (pod-db (pod-podcast-pod a-podcast))
                          "INSERT INTO tags (tag_term) VALUES ($1) RETURNING id"
                          (car rem)))))
            (query-exec
             (pod-db (pod-podcast-pod a-podcast))
             "INSERT INTO podcast_tags (tag_id, podcast_id) VALUES ($1, $2)"
             tag_id (pod-podcast-id a-podcast)))
          (loop (cdr rem))))))                

(define (insert-pod-episode! a-podcast ep)
  (define (date-str->sql-timestamp date-str)
    (query-value (pod-db (pod-podcast-pod a-podcast))
                 "[SELECT timestamp with time zone $1]"
                 date-str))
  (let ((the-ep (pod-episode (pod-podcast-pod a-podcast)
                         (query-value (pod-db (pod-podcast-pod a-podcast))
                                      (string-append
                                       "INSERT INTO episodes "
                                       "(file_url, title, description, date_published, podcast_id) "
                                       "VALUES ($1, $2, $3, $4, $5) "
                                       "RETURNING id")
                                      (hash-ref ep 'file_url)
                                      (hash-ref ep 'title)
                                      (hash-ref ep 'description)
                                      (query-value (pod-db (pod-podcast-pod a-podcast))
                                                   (format "SELECT timestamp with time zone '~a'" (hash-ref ep 'date_published)))
                                      (pod-podcast-id a-podcast)))))
    (insert-episode-tags! the-ep (hash-ref ep 'tags))))

(define (insert-pod-episodes! a-podcast eps)
  (for ((ep eps))
    (insert-pod-episode! a-podcast ep)))

(define (insert-episode-tags! an-episode tags)
  (let loop ((rem tags))
    (if (null? rem)
        (void)
        (begin
          (let ((tag_id (or
                         (query-maybe-value
                          (pod-db (pod-episode-pod an-episode))
                          "SELECT id FROM tags WHERE tag_term = $1"
                          (car rem))
                         (query-value
                          (pod-db (pod-episode-pod an-episode))
                          "INSERT INTO tags (tag_term) VALUES ($1) RETURNING id"
                          (car rem)))))
            (query-exec
             (pod-db (pod-episode-pod an-episode))
             "INSERT INTO podcast_tags (tag_id, podcast_id) VALUES ($1, $2)"
             tag_id (pod-episode-id an-episode)))
          (loop (cdr rem))))))

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
(define the-pod (init-pod!))

(provide pod pod-db the-pod pod-insert-podcast!)

(define clear
    (lambda () (clear-pod! "dolphin_pod" "dan")))
(define init
    (lambda () (init-pod! "dolphin_pod" "dan")))
