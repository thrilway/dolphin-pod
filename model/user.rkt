#lang racket/base
(require
  "structs.rkt"
  "profile.rkt"
  db
  racket/contract)

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

(provide (all-defined-out))