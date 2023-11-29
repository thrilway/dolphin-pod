#lang racket
(require net/url
         (prefix-in m: net/mime)
         (prefix-in g: gregor)
         racket/vector
         racket/date
         db/util/datetime
         db)

;Convert a string to a url safely
(define/contract (string->url/safe url-string (user-str '()))
  (->* (string?) ((or/c string? #f)) url?)
  (define/contract (extract-user-str)
    (-> (values string? (or/c string? #f)))
    (define rgxp #px"^(.*?//)(.*?@)?(.*)$")
    (let ((m (regexp-match rgxp url-string)))
      (if m
          (values (string-append (second m) (fourth m)) (third m))
          (values url-string #f))))
  (if (null? user-str)
      (call-with-values extract-user-str string->url/safe)
      (let ((start-url (string->url url-string)))
        (struct-copy url start-url (user user-str)))))
;resolve a feed url or return false
(define/contract (resolve-feed-url-or-fail feed-url (last-check 0) (seen-urls '()))
  (->* (url?) (integer? (listof url?)) (or/c string? url?))
  (define/contract (feed-mime? hdr-string)
    (-> string? boolean?)
    (define feed-types '(text application))
    (define feed-subtypes '(xml rss+xml atom+xml feed+json feed+xml))
    (let ((msg (m:mime-analyze (string->bytes/utf-8 hdr-string))))
      (if (and (member (m:entity-type (m:message-entity msg)) feed-types)
               (member (m:entity-subtype (m:message-entity msg)) feed-subtypes))
          #t
          #f)))  
  (define/contract (extract-status-code header-string)
    (-> string? number?)
    (let* ((code-line (car (string-split header-string "\r\n")))
           (m (regexp-match #px"^HTTP/1.1 (\\d*) .*$" code-line)))
      (string->number (second m) 10)))
  (define/contract (last-modified header-string)
    (-> string? integer?)
    (let loop ((fields (m:message-fields (call-with-input-string header-string m:mime-analyze))))
      (if (null? fields)
          0
          (let ((m (regexp-match #px"(?i:^last-modified:\\s*(\\S*)\\s*$)" (car fields))))
            (if m
                (rfc2822->seconds (second m))
                (loop (cdr fields)))))))
  (define/contract (extract-location header-string)
    (-> string? (or/c url? #f))
    (let loop ((fields (m:message-fields (call-with-input-string header-string m:mime-analyze))))
      (if (null? fields)
          #f
          (let ((m (regexp-match #px"(?i:^location:\\s*(\\S*)\\s*$)" (car fields))))
            (if m
                (string->url/safe (second m))
                (loop (cdr fields)))))))
  (if (member feed-url seen-urls)
      "Redirect Loop"
      (let-values (((_ headers) (get-pure-port/headers feed-url
                                                       #:method #"HEAD"
                                                       #:status? #t)))
        (let ((code (extract-status-code headers)))
          (cond ((and
                  (< code 300)
                  (>= code 200))
                 (cond ((not (feed-mime? headers)) "Not a feed")
                       ((< (last-modified headers) last-check) "Unchanged")
                       (else feed-url)))
                ((= code 302) "Unchanged")
                ((and
                  (< code 400)
                  (>= code 300))
                 (let ((next-url (extract-location headers)))
                   (if next-url
                       (resolve-feed-url-or-fail next-url last-check (cons feed-url seen-urls))
                       "Bad Redirect")))
                (else "Not Found"))))))

(define/contract (clean-xml xml-str)
  (-> string? string?)
  (define entity-regexp #px"^&([:alpha:]*);|(#x[:xdigit:]*);|(#[:digit:]*);")
  (let loop ((idx 0) (str xml-str))
    (let ((m-pos (regexp-match-positions #rx"&" str idx)))
      (if m-pos
          (let* ((start (caar m-pos))
                 (ent-m (regexp-match entity-regexp str start)))
            (if ent-m
                (loop (add1 start) str)
                (loop (+ start 5) (string-append
                                 (substring str 0 start)
                                 "&amp;"
                                 (substring str (add1 start) (string-length str))))))
          str))))
; Convert an RFC2822 formatted time-string to a sql-timestamp (with tz)
;(define/contract (rfc2822->datetime datetime/string)
;  (-> string? (or/c #f g:datetime?))
;   (for/or ((pattern (list
;                      "EEE, dd MMM y HH:mm:ss xxxx"
;                      "EEE, dd MMM y HH:mm:ss zzz"
;                      "dd MMMM y HH:mm:ss xxxx"
;                      "dd MMMM y HH:mm:ss zzz"
;                      "EEE, dd MMMM y HH:mm:ss xxxx"
;                      "EEE, dd MMMM y HH:mm:ss zzz"
;                      "dd MMMM y HH:mm:ss xxxx"
;                      "dd MMMM y HH:mm:ss zzz")))
;      (with-handlers ((g:exn:gregor:parse? (lambda (exn) #f)))
;        (g:parse-datetime datetime/string pattern))))
(define/contract (rfc2822->sql-timestampz datetime/string)
  (-> string? (or/c #f (and/c sql-timestamp? sql-timestamp-tz)))
  (define months (vector 0 "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  (define zones (hash "GMT" "-0000"
                      "EST" "-0500"
                      "EDT" "-0400"
                      "AST" "-0400"
                      "ADT" "-0500"
                      "NST" "-0330"
                      "NDT" "-0230"
                      "CST" "-0600"
                      "CDT" "-0500"
                      "MST" "-0700"
                      "MDT" "-0600"
                      "PST" "-0800"
                      "PDT" "-0700"))
  (define rgx
    (pregexp
     (string-append
      "^"
      "(Mon|Tue|Wed|Thu|Fri|Sat|Sun), "
      "(\\d{,2}) "
      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) "
      "(\\d{4}) "
      "(\\d{2}):(\\d{2}):(\\d{2}) "
      "(\\+\\d{4}|\\-\\d{4}|[[:upper:]]{3})"
      "\\s*$")))
  (define/contract (parse-zone tzstring)
    (-> string? integer?)
    (let ((m (regexp-match #px"^\\+\\d{4}$|^\\-\\d{4}$" tzstring)))
      (if m
          (let ((sign (string->number (format "~a1" (substring tzstring 0 1))))
                (hours (string->number (substring tzstring 1 3)))
                (minutes (string->number (substring tzstring 3))))
            (* sign (+ minutes (* hours 60))))
          (parse-zone (hash-ref zones tzstring)))))
  (let ((m (regexp-match rgx datetime/string)))
    (if (and (list? m) (= (length m) 9))
        (let ((year (string->number (list-ref m 4)))
              (month (vector-member (list-ref m 3) months))
              (day (string->number (list-ref m 2)))
              (hour (string->number (list-ref m 5)))
              (minute (string->number (list-ref m 6)))
              (second (string->number (list-ref m 7)))
              (zone (parse-zone (list-ref m 8))))
          (sql-timestamp year month day hour minute second 0 zone))
        (raise-user-error 'time-parsing "Could not parse ~s." datetime/string))))
(define/contract (sql-timestampz->date* tsz)
  (-> (or/c sql-null? sql-timestamp?) date*?)
  (if (sql-null? tsz)
      (seconds->date 0 #f)
      (sql-datetime->srfi-date tsz)))
(define/contract (sql-timestampz->seconds tsz)
  (-> (or/c sql-null? sql-timestamp?) integer?)
  (if (sql-null? tsz)
      0
      (date*->seconds (sql-datetime->srfi-date tsz))))
(define/contract (rfc2822->date* date/string)
  (-> string? date*?)
  (sql-timestampz->date* (rfc2822->sql-timestampz date/string)))
(define/contract (rfc2822->seconds date/string)
  (-> string? integer?)
  (date*->seconds (rfc2822->date* date/string)))
(define/contract (seconds->rfc2822 s)
  (-> integer? string?)
  (date-display-format 'rfc2822)
  (let ((str (date->string (seconds->date s #f) #t)))
    (string-replace str "+0000" "GMT")))
  
(provide string->url/safe
         resolve-feed-url-or-fail
         clean-xml
         rfc2822->sql-timestampz
         rfc2822->date*
         rfc2822->seconds
         sql-timestampz->seconds
         sql-timestampz->date*
         seconds->rfc2822
         )
(module+ test
  (require rackunit)
  (check-equal? (sql-timestamp 2021 1 25 20 0 0 0 0)
               (rfc2822->sql-timestampz "Mon, 25 Jan 2021 20:00:00 GMT"))
  (check-equal? (sql-timestamp 2023 11 15 0 10 0 0 -300)
                (rfc2822->sql-timestampz "Wed, 15 Nov 2023 00:10:00 EST")))
;(define-values (_ headers) (get-pure-port/headers (string->url/safe "https://Milway:7*fp#l&SRY8r@pardcast.com/feeds/season19/season19.xml")
;                                                  #:method #"HEAD"
;                                                  #:status? #t))