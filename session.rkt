#lang racket/base
(require web-server/http
         web-server/http/id-cookie
         (only-in net/cookies/server cookie?)
         racket/contract
         libuuid
         "model.rkt")

(define sessions (make-hash))

(define salt (make-secret-salt/file "salt"))

(define/contract (get-session-user req)
  (-> request? (or/c #f pod-user?))
  (let ((sid (request-id-cookie req
                                #:name "RACKETSESSION"
                                #:key salt)))
    (if sid
        (hash-ref sessions sid #f)
        #f)))

(define/contract (make-session/cookie! a-user)
  (-> pod-user? cookie?)
  (let ((sid (uuid-generate/random))
        (expiry-date (seconds->date (+ 86400 (current-seconds)))))
    (hash-set! sessions sid a-user)
    (make-id-cookie "RACKETSESSION"
                    sid
                    #:key salt
                    #:expires expiry-date
                    #:http-only? #t #:secure? #t)))

(provide get-session-user make-session/cookie!)