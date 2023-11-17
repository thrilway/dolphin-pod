#lang racket/base
(require
  racket/contract
  db
  db/util/datetime  
  net/url
  "workers.rkt"
  racket/match
  racket/date
  dotenv)


(define/contract (getenv-or-die var)
  (-> string? string?)
  (unless (string? var)
    (error "Variable should be a string:" var))
  (define val (getenv var))
  (unless (string? val)
    (error (format "Mandatory environment variable \"~a\" is unset." var)))
  val)
(define/contract (getenv-or-false var)
  (-> string? (or/c string? #f))
  (unless (string? var)
    (error "Variable should be a string:" var))
  (let ((val (getenv var)))
    (if (string? val)
        val
        #f)))

(define/contract (load-db)
  (-> data-source?)
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
(define/contract (connect!)
  (-> connection?)
  (dsn-connect db-source))
;Structs
(struct/contract pod ((db connection?)))
(struct/contract pod-tag ((pod pod?) (id integer?)))
(struct/contract pod-podcast ((pod pod?) (id number?)))
(struct/contract pod-author ((pod pod?) (id integer?)))
(struct/contract pod-episode ((pod pod?) (id integer?)))
(struct/contract pod-user ((pod pod?) (id number?)))
(struct/contract pod-profile ((pod pod?) (id number?)))
(struct/contract pod-review ((pod pod?) (id number?)))
(struct/contract pod-comment ((pod pod?) (id integer?)))
;The Pod
(define/contract (init-pod!)
  (-> pod?)
  (define db
    (virtual-connection
     (connection-pool connect!)))
  (define the-pod (pod db))
  (unless (table-exists? (pod-db the-pod) "users")
    (init-pod-users! the-pod))
  (unless (table-exists? (pod-db the-pod) "podcasts")
    (init-pod-podcasts! the-pod))
  (unless (table-exists? (pod-db the-pod) "episodes")
    (init-pod-episodes! the-pod))
  (unless (table-exists? (pod-db the-pod) "tags")
    (init-pod-tags! the-pod))
  (unless (table-exists? (pod-db the-pod) "podcast_reviews")
    (init-pod-reviews! the-pod))
  (unless (table-exists? (pod-db the-pod) "comments")
    (init-pod-comments! the-pod))
  (unless (table-exists? (pod-db the-pod) "subscriptions")
    (init-pod-subscriptions! the-pod))
  (unless (table-exists? (pod-db the-pod) "profile_episode_rels")
    (init-pod-profile-episode-rels! the-pod))
  (unless (table-exists? (pod-db the-pod) "profile_entity_stars")
    (init-pod-stars! the-pod))
  (unless (table-exists? (pod-db the-pod) "profile_entity_shares")
    (init-pod-shares! the-pod))
  (unless (table-exists? (pod-db the-pod) "tag_entity_rels")
    (init-pod-tag-entity-rels! the-pod))
  the-pod)


;(define/contract (clear-pod! database user #:password (password #f) #:socket (socket 'guess))
;  (->* (connection? string?) (string? symbol?) void?)
;    (define db (postgresql-connect
;              #:user user
;              #:database database
;              #:socket 'guess))
;  (query-exec db
;              (string-append
;               "DROP TABLE IF EXISTS "
;               "users, "
;               "podcasts, "
;               "episodes "
;               "CASCADE")))
;Entities
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
;Tags
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
(define/contract (init-pod-tag-entity-rels! a-pod)
  (-> pod? void?)
  (unless (table-exists? (pod-db a-pod) "entities")
    (init-pod-podcasts! a-pod))
  (unless (table-exists? (pod-db a-pod) "tags")
    (init-pod-tags! a-pod))
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE tag_entity_rels ("
               "id bigserial PRIMARY KEY, "
               "tag_id integer REFERENCES tags (id) ON DELETE CASCADE, "
               "entity_id bigint REFERENCES entities (id) ON DELETE CASCADE)")))
(define/contract (pod-insert-tag-entity-rel! a-pod a-tag an-entity)
  (-> pod? pod-tag? pod-entity? void?)
  (query-exec (pod-db a-pod)
              "INSERT INTO tag_entity_rels (tag_id, entity_id) VALUES ($1, $2)"
              (pod-tag-id a-tag)
              (pod-entity-id an-entity)))
;Podcasts
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
               "explicit boolean NOT NULL, "
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
                            (hash-ref podcast-info 'explicit #t)
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
(define/contract (pod-podcast-explicit? a-podcast)
  (-> pod-podcast? boolean?)
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

;Episodes
(define/contract (init-pod-episodes! a-pod)
  (-> pod? void?)
  (unless (table-exists? (pod-db a-pod) "podcasts")
    (init-pod-podcasts! a-pod))
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
                                    (raise-user-error 'pod-insert-episode "Podcast: ~a\nEpisode: ~a\n~a\nSQLSTATE: ~a\n"
                                                      (pod-podcast-title a-podcast)
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
;Images
(define/contract (init-pod-images! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE images ("
               "id serial PRIMARY KEY, "
               "image bytea NOT NULL, "
               "mime_type varchar(50) NOT NULL)")))

;Users and Profiles
(define/contract (init-pod-profiles! a-pod)
  (-> pod? void?)
  (unless (table-exists? (pod-db a-pod) "images")
    (init-pod-images! a-pod))
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE profiles ("
               "id serial PRIMARY KEY, "
               "name text, "
               "username text NOT NULL UNIQUE, "
               "icon_id integer REFERENCES images (id) ON DELETE CASCADE, "
               "icon_alt text, "
               "image_id integer REFERENCES images (id) ON DELETE CASCADE, "
               "image_alt text, "
               "description text, "
               "manually_approves_follows boolean DEFAULT true,"
               "created_on timestamp with time zone DEFAULT CURRENT_TIMESTAMP)")))
(define/contract (init-pod-users! a-pod)
  (-> pod? void?)
  (unless (table-exists? (pod-db a-pod) "profiles")
    (init-pod-profiles! a-pod))
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE users ("
               "id serial PRIMARY KEY, "
               "email text NOT NULL UNIQUE, "
               "password text NOT NULL, "
               "profile_id integer REFERENCES profiles (id) ON DELETE CASCADE)")))

(define/contract (pod-insert-user! a-pod username email password)
  (-> pod? string? string? string? pod-user?)
  (define email-exists?
    (if
     (query-maybe-value (pod-db a-pod)
                        (string-append
                         "SELECT id FROM users "
                         "WHERE email ILIKE $1")
                        email)
     #t
     #f))
  (define username-exists?
    (if
     (query-maybe-value (pod-db a-pod)
                        (string-append
                         "SELECT id FROM profiles "
                         "WHERE username ILIKE $1")
                        username)
     #t
     #f))
  (cond
    ((and email-exists? username-exists?)
     (raise-user-error 'user-exists))
    (email-exists? (raise-user-error 'email-exists))
    (username-exists? (raise-user-error 'username-exists))
    (else
     (let ((profile-id (query-value (pod-db a-pod)
                                    (string-append
                                     "INSERT INTO profiles "
                                     "(username) "
                                     "VALUES ($1) "
                                     "RETURNING id")
                                    username
                                    )))
       (pod-user a-pod
                 (query-value (pod-db a-pod)
                              (string-append
                               "INSERT INTO users "
                               "(email, password, profile_id) "
                               "VALUES ($1, crypt($2, gen_salt('bf', 8)), $3) "
                               "RETURNING id")
                              email
                              password
                              profile-id))))))
(define/contract (pod-user-login a-pod username-or-email password)
  (-> pod? string? string? (or/c #f pod-user?))
  (let ((uid
         (query-maybe-value (pod-db a-pod)
                            (string-append
                             "SELECT users.id FROM "
                             "users JOIN profiles ON users.profile_id = profile.id "
                             "WHERE ( profiles.username = $1 OR users.email = $1 ) "
                             "AND users.password = crypt($2, password)")
                            username-or-email password)))
    (if uid
        (pod-user a-pod uid)
        #f)))
(define/contract (pod-user-email a-user)
  (-> pod-user? string?)
  (query-value (pod-db (pod-user-pod a-user))
               "SELECT email FROM users WHERE id = $1"
               (pod-user-id a-user)))
(define/contract (pod-user-profile a-user)
  (-> pod-user? pod-profile?)
  (pod-profile (pod-user-pod a-user)
               (query-value (pod-db (pod-user-pod a-user))
                            "SELECT profile_id FROM users WHERE id = $1"
                            (pod-user-id a-user))))
(define/contract (pod-profile-username a-profile)
  (-> pod-profile? string?)
  (query-value (pod-db (pod-profile-pod a-profile))
               "SELECT username FROM profiles WHERE id = $1"
               (pod-profile-id a-profile)))
(define/contract (pod-profile-name a-profile)
  (-> pod-profile? (or/c #f string?))
  (query-maybe-value (pod-db (pod-profile-pod a-profile))
                     "SELECT name FROM profiles WHERE id = $1"
                     (pod-profile-id a-profile)))
(define/contract (pod-profile-description a-profile)
  (-> pod-profile? (or/c #f string?))
  (query-maybe-value (pod-db (pod-profile-pod a-profile))
                     "SELECT description FROM profiles WHERE id = $1"
                     (pod-profile-id a-profile)))
(define/contract (pod-profile-icon a-profile)
  (-> pod-profile? (or/c #f bytes?))
  (query-maybe-value (pod-db (pod-profile-pod a-profile))
                     (string-append
                      "SELECT images.image FROM "
                      "profiles JOIN images "
                      "ON profiles.icon_id = images.id "
                      "WHERE profiles.id = $1")
                     (pod-profile-id a-profile)))
(define/contract (pod-profile-icon-mimetype a-profile)
  (-> pod-profile? (or/c #f string?))
  (query-maybe-value (pod-db (pod-profile-pod a-profile))
                     (string-append
                      "SELECT images.mime_type FROM "
                      "profiles JOIN images "
                      "ON profiles.icon_id = images.id "
                      "WHERE profiles.id = $1")
                     (pod-profile-id a-profile)))
(define/contract (pod-profile-image a-profile)
  (-> pod-profile? (or/c #f bytes?))
  (query-maybe-value (pod-db (pod-profile-pod a-profile))
                     (string-append
                      "SELECT images.image FROM "
                      "profiles JOIN images "
                      "ON profiles.image_id = images.id "
                      "WHERE profiles.id = $1")
                     (pod-profile-id a-profile)))
(define/contract (pod-profile-image-mimetype a-profile)
  (-> pod-profile? (or/c #f string?))
  (query-maybe-value (pod-db (pod-profile-pod a-profile))
                     (string-append
                      "SELECT images.mime_type FROM "
                      "profiles JOIN images "
                      "ON profiles.image_id = images.id "
                      "WHERE profiles.id = $1")
                     (pod-profile-id a-profile)))
(define/contract (pod-profile-manually-approves-follows? a-profile)
  (-> pod-profile? boolean?)
  (query-value (pod-db (pod-profile-pod a-profile))
               "SELECT manually_approves_follows FROM profiles WHERE id = $1"
               (pod-profile-id a-profile)))
;Reviews
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

;Comments
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

;Relations
(define/contract (init-pod-subscriptions! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE subscriptions ("
               "id serial PRIMARY KEY, "
               "podcast_id integer REFERENCES podcasts (id) ON DELETE CASCADE, "
               "profile_id integer REFERENCES profiles (id) ON DELETE CASCADE, "
               "created_on timestamp with time zone DEFAULT CURRENT_TIMESTAMP)")))
(define/contract (pod-subscribe! a-pod a-profile a-podcast)
  (-> pod? pod-profile? pod-podcast? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "INSERT INTO subscriptions (podcast_id, profile_id) "
               "VALUES ($1, $2)"
               )
              (pod-podcast-id a-podcast)
              (pod-profile-id a-profile)))
(define/contract (pod-profile-subscriptions/by-latest-episode a-pod a-profile)
  (-> pod? pod-profile? (listof pod-podcast?))
  (let ((rows
         (query-rows (pod-db a-pod)
                     (string-append
                      "SELECT podcast_id, max(episodes.date_posted) AS latest_episode_date"
                      "FROM subscriptions JOIN episodes USING (podcast_id) "
                      "WHERE subscriptions.profile_id = $1 "
                      "GROUP BY podcast_id "
                      "ORDER BY max(episodes.date_posted) DESC")
                     (pod-profile-id a-profile))))
    (map (lambda (row) (pod-podcast (a-pod) (vector-ref row 0))) rows)))

(define/contract (init-pod-profile-episode-rels! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE profile_episode_rels ("
               "id serial PRIMARY KEY, "
               "episode_id integer REFERENCES episodes (id) ON DELETE CASCADE, "
               "profile_id integer REFERENCES profiles (id) ON DELETE CASCADE, "
               "seconds_listened integer, "
               "listened boolean, "
               "starred_on timestamp with time zone DEFAULT NULL, "
               "shared_on timestamp with time zone DEFAULT NULL)")))
(define/contract (init-pod-stars! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE profile_entity_stars ("
               "id serial PRIMARY KEY, "
               "profile_id integer REFERENCES profiles (id) ON DELETE CASCADE, "
               "entity_id bigint REFERENCES entities (id) ON DELETE CASCADE, "
               "starred_on timestamp with time zone DEFAULT CURRENT_TIMESTAMP)")))
(define/contract (init-pod-shares! a-pod)
  (-> pod? void?)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE profile_entity_shares ("
               "id serial PRIMARY KEY, "
               "profile_id integer REFERENCES profiles (id) ON DELETE CASCADE, "
               "entity_id bigint REFERENCES entities (id) ON DELETE CASCADE, "
               "shared_on timestamp with time zone DEFAULT CURRENT_TIMESTAMP)")))
(module* main #f
  (require racket/port)
  (define the-pod (init-pod!))
  (define feeds (import-opml (port->bytes (open-input-file "/home/dan/Downloads/PocketCasts.opml"))))
  (for ((feed feeds))
    (if (string? (cdr feed))
        (void)
        (pod-insert-podcast! the-pod (cdr feed))))
  )