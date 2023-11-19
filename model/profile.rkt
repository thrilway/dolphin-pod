#lang racket/base
(require db
         racket/contract
         "structs.rkt"
         "images.rkt")
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

(provide (all-defined-out))