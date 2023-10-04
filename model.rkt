#lang web-server
(require db)

(define conn (sqlite3-connect #:database "dp.db"))

(define (get-webfinger bindings host)
  (define user-name
    (let ((acct (regexp-match #rx"^acct:(.*)@.*$" (cdr (assq 'resource bindings)))))
      (if acct
          (list-ref acct 1)
          '())))
  (if user-name
      (hash 'subject (cdr (assq 'resource bindings))
            'aliases (list (format "~a/~a" host user-name)
                           (format "~a/user/~a" host user-name)))
      '()))

(provide get-webfinger)