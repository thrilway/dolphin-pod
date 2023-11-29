#lang racket/base
(require racket/date
         racket/contract
         racket/string
         libuuid
         net/sendmail
         net/head
         net/url
         web-server/templates
         dotenv
         "model.rkt")

(define started-signups (make-hash))

(struct/contract signup ((user pod-user?)
                         (expiry integer?)))

(define/contract (start-signup! a-pod username email password)
  (-> pod? string? string? string? void?)
  (let* ((expiry-date (+ 7200 (current-seconds)))
         (vid (uuid-generate))
         (the-user (pod-insert-user! a-pod username email password))
         (s (signup the-user expiry-date)))
    (begin
      (hash-set! started-signups vid s)
      (send-verification-email vid s))))
(define/contract (finish-signup! a-pod ver-str)
  (-> pod? string? (or/c #f pod-user?))
  (define signup-or-false (hash-ref started-signups ver-str #f))
  (cond ((and signup-or-false
              (< (signup-expiry signup-or-false) (current-seconds)))
         (begin
           (pod-user-verify! (signup-user signup-or-false))
           (hash-remove! started-signups ver-str)
           (signup-user signup-or-false)))
        (signup-or-false ;link-expired
         (begin 
             (hash-remove! started-signups ver-str)
             (pod-user-remove! (signup-user signup-or-false))))
        (else #f) ;invalid link
        ))

(define/contract (send-verification-email a-uuid a-signup)
  (-> string? signup? void?)
  (define magic-url (url "http" #f (getenv "SERVER_HOST") (string->number (getenv "SERVER_PORT")) #t (list (path/param "verify-signup" '()) (path/param a-uuid '())) '() #f))
  (define body (string-split (include-template "view/verify-email.html") "\n"))
  (define head (standard-message-header (format "no-reply@~a" (getenv "SERVER_HOST"))
                                        (list (pod-user-email (signup-user a-signup)))
                                        '()
                                        '()
                                        "New Dolphin Pod signup!"))
  (send-mail-message (extract-field "FROM" head)
                     (extract-field "TO" head)
                     "New Dolphin Pod signup!"
                     '()
                     '()
                     body))

(provide start-signup! finish-signup!)