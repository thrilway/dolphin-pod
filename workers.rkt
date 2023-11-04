#lang racket/base
(require json
         net/url
         racket/port
         racket/string
         racket/file)

(define (get-podcast-info feed-url)
  (define (fetch-image url-string)
    (port->bytes (get-pure-port (string->url url-string))))
  (define (time-str->secs time-str)
    (let loop ((rem (reverse (map string->number (string-split time-str ":")))) (acc 0))
      (if (null? rem)
          acc
          (loop (map (lambda (x) (* 60 x)) (cdr rem)) (+ acc (car rem))))))
  (define-values (sp out in err) (subprocess #f #f #f "./workers/get_podcast.py" feed-url))
  (let ((js (read-json out)))
    (let ((new-js (hash-set js 'cover_art (fetch-image (hash-ref js 'cover_art_url)))))
      (for/fold ((out '())
                 #:result (hash-set new-js 'episodes out))
                ((h (hash-ref new-js 'episodes)))
        (append out
                (list
                 (hash-set h 'duration (time-str->secs (hash-ref h 'duration)))))))))

(define (import-opml opml-file)
  (let ((temp-file-name (make-temporary-file)))
    (begin
      (call-with-output-file temp-file-name
        (lambda (out)
          (write-bytes opml-file out))
        #:exists 'replace)
      (let-values (((sp out in err) (subprocess #f #f #f "./workers/import_opml.py" temp-file-name)))
        (let ((js (read-json out)))
          (begin
            (delete-file temp-file-name)
            js))))))
    
(provide get-podcast-info import-opml)
