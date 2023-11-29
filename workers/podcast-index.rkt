#lang racket/base
(require net/url
         net/http-client
         json
         crypto
         crypto/libcrypto
         racket/contract)

(crypto-factories (list libcrypto-factory))
(define api-key "RCBZZYUBMJGSSFHGEPGS")
(define api-secret "D6GMDSr63h4jP3KhxNfNENXSFX8V$zYQD4JeCrqD")
(define user-agent "DolphinPod/0.0.1")

(define/contract (podcast-by-feed-url/string feed-url/string)
  (-> string? jsexpr?)
   (define/contract (bytes->hex bstr)
    (-> bytes? string?)
    (let loop ((byte-list (bytes->list bstr)) (acc ""))
      (if (null? byte-list)
          acc
          (loop (cdr byte-list) (string-append acc
                                               (number->string (car byte-list) 16))))))
  (define base-url (string->url "https://api.podcastindex.org/api/1.0/podcasts/byfeedurl"))
  (define (headers)
    (let ((date (current-seconds)))
      (list (format "User-Agent: ~a" user-agent)
            (format "X-Auth-Date: ~a" date)
            (format "X-Auth-Key: ~a" api-key)
            (format "Authorization: ~a" (bytes->hex (digest 'sha1 (string-append api-key api-secret (number->string date))))))))
  (let ((full-url (struct-copy url base-url (query (list (cons 'url feed-url/string))))))
      (read-json (get-pure-port full-url (headers)))))

(define pcast (podcast-by-feed-url/string "https://feeds.simplecast.com/byb4nhvN"))