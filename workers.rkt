#lang racket/base
(require net/url
         xml
         xml/path
         racket/port)

(define (get-podcast-info feed-url)
  (define feed-xexpr (xml->xexpr (document-element (read-xml (get-pure-port (string->url feed-url))))))
  (define podcast-info
    (let ((title (se-path* '(channel title) feed-xexpr))
          (description (se-path* '(channel description) feed-xexpr))
          (image-url (string->url (se-path* '(channel image url) feed-xexpr))))
      (hash 'title title
            'description description
            'cover_art (port->bytes (get-pure-port image-url)))))
  (define episodes-info
    (let ((titles (se-path*/list '(channel item title) feed-xexpr))
          (descriptions (se-path*/list '(channel item description) feed-xexpr))
          (dates-published (se-path*/list '(channel item pubDate) feed-xexpr))
          (urls (se-path*/list '(channel item enclosure #:url) feed-xexpr))
          (durations (se-path*/list '(channel item enclosure #:length) feed-xexpr))
          (types (se-path*/list '(channel item enclosure #:type) feed-xexpr)))
      (map
       (lambda (title descr pub url dur type)
         (hash 'title title
               'description descr
               'date_published pub
               'file_url url
               'duration dur
               'mime_type type))
       titles descriptions
       dates-published urls
       durations types)))
  (values podcast-info episodes-info))

(provide get-podcast-info)