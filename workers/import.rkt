#lang racket
(require (prefix-in x: xml)
         net/url
         "utils.rkt")


(define/contract (import-opml opml-bytes)
  (-> bytes? (listof (cons/c string? (or/c url? string?))))
  (let* ((opml-str (clean-xml (bytes->string/utf-8 opml-bytes)))
         (feed-url-alist (extract-feeds-from-opml-xexpr (x:string->xexpr opml-str))))
    (for/fold ((out '())
               #:result out)
              ((url-assoc feed-url-alist))
      (append out (list (cons (car url-assoc) (resolve-feed-url-or-fail (string->url/safe (cdr url-assoc)))))))))

(define/contract (extract-feeds-from-opml-xexpr xml-xexpr)
  (-> x:xexpr? (listof (cons/c string? string?)))
  (match xml-xexpr
    ((list 'outline (list (list attr-names attr-values) ...))
     (let ((attrs (map cons attr-names attr-values)))
       (list
        (cons (cdr (assoc 'text attrs))
              (cdr (assoc 'xmlUrl attrs))))))
    ((list _ (list (list _ _) ...) content ...)
     (apply append (map extract-feeds-from-opml-xexpr content)))
    (_ '())))

(define/contract (extract-feeds-from-content xml-content)
  (-> x:content/c (listof (cons/c string? string?)))
  (match xml-content
    ((struct x:element (start stop 'outline attributes '()))
     (list
      (cons (extract-title-from-attributes attributes)
            (extract-feed-from-attributes attributes))))
    ((struct x:element (start stop name attributes content))
     (apply append (map extract-feeds-from-content content)))
    (_ '())))

(define/contract (extract-feed-from-attributes attrs)
  (-> (listof x:attribute?) string?)
  (if (string=? (x:attributes-ref attrs 'type) "rss")
      (list (x:attributes-ref attrs 'xmlUrl))
      '()))

(define/contract (extract-title-from-attributes attrs)
  (-> (listof x:attribute?) string?)
  (if (string=? (x:attributes-ref attrs 'type) "rss")
      (list (x:attributes-ref attrs 'text))
      '()))


(define/contract (x:attributes-ref attrs name)
  (-> (listof x:attribute?) symbol? any/c)
  (let ((goal-attr (findf (lambda (attr) (eqv? name (x:attribute-name attr))) attrs)))
    (if goal-attr
        (x:attribute-value goal-attr)
        #f)))
(provide import-opml)