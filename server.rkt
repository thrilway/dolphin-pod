#lang racket

(require web-server/servlet
         web-server/formlets
         json xml xml/path
         ;"webfinger.rkt"
         "model.rkt"
         "workers.rkt")

(define/contract (home req)
  (-> request? response?)
  (render-dolphin-pod the-pod req))
(define/contract (opml-import req)
  (-> request? response?)
  (render-opml-import-page the-pod req))
(define/contract (opml-upload req)
  (-> request? response?)
    (begin
      (let-values (((fname fcontents) (formlet-process opml-import-formlet req)))
        (process-opml the-pod fcontents))
      (response/empty #:code 303
                      #:headers (list
                                 (header #"Location" (string->bytes/utf-8 (url-generator home)))))))
(define/contract (subscribe-form-get req)
  (-> request? response?)
  (render-subscribe-page the-pod req))
(define/contract (subscribe-form-post req)
  (-> request? response?)
  (let ((feed-url (formlet-process feed-subscribe-formlet req)))
    (pod-insert-podcast! the-pod feed-url)
    (response/empty #:code 303
                      #:headers (list
                                 (header #"Location" (string->bytes/utf-8 (url-generator home)))))))
(define-values (dispatch url-generator)
  (dispatch-rules
   (("") home)
   (("import") #:method "get" opml-import)
   (("import") #:method "post" opml-upload)
   (("subscribe") #:method "get" subscribe-form-get)
   (("subscribe") #:method "post" subscribe-form-post)
   ;(("podcasts") podcasts-page-handler)
   ))

(define/contract (render-dolphin-pod a-pod request)
  (-> pod? request? response?)
  (response/xexpr
   `(html
     (head
      (title "Dolphin Pod: a social podcatcher"))
     (body
      (h3 "Currently available functions:")
      (ul
       (li (a ((href ,(url-generator opml-import))) "Import an OPML file"))
       (li (a ((href ,(url-generator subscribe-form-get))) "Subscribe to an RSS feed")))))))

(define/contract (render-opml-import-page a-pod req)
  (-> pod? request? response?)
  (response/xexpr
   `(html
     (head
      (title "Import and OPML file"))
     (body
      (form
       ((action
         ,(url-generator opml-upload))
        (method "POST")
        (enctype "multipart/form-data"))
       ,@(formlet-display opml-import-formlet)
       (input ((type "submit"))))))))
  
(define opml-import-formlet
  (formlet
   (div
    ,{(file-upload #:attributes (list (list 'accept "text/x-opml"))) . => . binds})
   (let
       ([fname (bytes->string/utf-8 (binding:file-filename binds))]
        [fcontents (binding:file-content binds)])
   (values fname fcontents))))

(define/contract (process-opml a-pod a-file)
  (-> pod? bytes? void?)
  (let loop ((rem (import-opml a-file)))
    (if (null? rem)
        (void)
        (begin
          (pod-insert-podcast! a-pod (hash-ref (car rem) 'feedUrl))
          (loop (cdr rem))))))

(define/contract (render-subscribe-page a-pod req)
  (-> pod? request? response?)
  (response/xexpr
   `(html
     (head
      (title "Subscribe to a podcast"))
     (body
      (form ((action ,(url-generator subscribe-form-post))
             (method "POST"))
            ,@(formlet-display feed-subscribe-formlet)
            (input ((type "submit"))))))))
(define feed-subscribe-formlet
  (formlet
   (div
    ,{input-string . => . feed-url})
   feed-url))

   
(require web-server/servlet-env)
(serve/servlet dispatch
               #:launch-browser? #f
               #:quit? #t
               #:listen-ip #f
               #:port 8080
               #:extra-files-paths
               (list (build-path (current-directory-for-user) "htdocs")
                     (build-path (current-directory-for-user) "workers"))
               #:servlet-regexp #rx""
               #:log-file "./soc.log"
               #:servlet-path "")
