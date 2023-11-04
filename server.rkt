#lang racket

(require web-server/servlet
         web-server/formlets
         json
         ;"webfinger.rkt"
         "model.rkt"
         "workers.rkt")

(provide/contract (start (request? . -> . response?)))

(define (start req)
  (render-dolphin-pod the-pod req))

(define (render-dolphin-pod a-pod request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head
        (title "Dolphin Pod: a social podcatcher"))
       (body
        (h3 "Currently available functions:")
        (ul
         (li (a ((href ,(embed/url opml-import-page-handler))) "Import an OPML file")))))))
  (define (opml-import-page-handler req)
    (render-opml-import-page a-pod req))
  (send/suspend/dispatch response-generator))

(define (render-opml-import-page a-pod req)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head
        (title "Import and OPML file"))
       (body
        (form
         ((action
           ,(embed/url opml-upload-handler))
	   (method "POST")
	   (enctype "multipart/form-data"))
         ,@(formlet-display opml-import-formlet)
	 (input ((type "submit"))))))))
  (define (opml-upload-handler req)
    (begin
      (process-opml a-pod (formlet-process opml-import-formlet req))
      (render-dolphin-pod a-pod (redirect/get))))
  (send/suspend/dispatch response-generator))
  
(define opml-import-formlet
  (formlet
   (#%#
    ,((file-upload #:attributes (list (list 'accept "text/x-opml"))) . => . opml-file))
   opml-file))

(define (process-opml a-pod a-file)
  (let loop ((rem (import-opml a-file)))
    (if (null? rem)
        (void)
        (begin
          (thread
           (lambda () (pod-insert-podcast! a-pod (hash-ref (car rem) 'feedUrl))))
          (loop (cdr rem))))))
        
(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8080
               #:extra-files-paths
               (list (build-path (current-directory) "htdocs"))
               #:servlet-regexp #rx""
               #:log-file "./soc.log"
               #:servlet-path "")
