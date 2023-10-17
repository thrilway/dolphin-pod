#lang web-server

(require web-server/http
         web-server/managers/none
         json
         web-server/servlet-env
         "webfinger.rkt")
(provide interface-version manager start)
 
(define interface-version 'v2)
(define manager
  (create-none-manager
   (lambda (req)
     (response/xexpr
      `(html (head (title "No Continuations Here!"))
             (body (h1 "No Continuations Here!")))))))
(define (start req)
  (soc-dispatch req))

(define-values (soc-dispatch soc-urls)
  (dispatch-rules
   (("") home)
   ((".well-known" "webfinger") webfinger)
   (("user" (string-arg)) #:method "get" user)
   (("user" (string-arg) "outbox") #:method "post" user-outbox-post)
   (("user" (string-arg) "outbox") #:method "get" user-outbox-get)
   (("user" (string-arg) "inbox") #:method "post" user-inbox-post)
   (("user" (string-arg) "inbox") #:method "get" user-inbox-get)
   (("user" (string-arg) (integer-arg)) #:method "get" review-get)
   (("podcast" (integer-arg)) #:method get podcast-get)
   (("episode" (integer-arg)) #:method get episode-get)
   ))

(define (user-outbox-post req user)
  (define (authorized? req) #t)
  (if (authorized? req)
      (let ((data ((bytes->jsexpr (request-post-data/raw)))
            (id-num ()
        (cond ((AS-Object? data)
               (let ((create (make-hash))
               ))))

(define (home req)
  (define (render-headers req)
    (for/fold ((out '())
               #:result out)
              ((header (request-headers req)))
      (append out (list `(div ,(string-append (symbol->string (car header)) ": " (cdr header)))))))
  (define (render-bindings req)
    (for/fold ((out '())
               #:result out)
              ((binding (request-bindings req)))
      (append out (list `(div ,(string-append (symbol->string (car binding)) ": " (cdr binding)))))))
  (begin
    (display req)
    (response/xexpr
     `(html
       (head (title "Soc"))
       (body
        (table
         (tr ((valign "top"))
          (td "URI: ")
          (td ,(url->string (request-uri req))))
         (tr ((valign "top"))
          (td "Headers: ")
          (td ,@(render-headers req)))
         (tr ((valign "top"))
          (td "Bindings: ")
          (td ,@(render-bindings req)))))))))


(serve/servlet start
               #:port 8080
               #:servlet-regexp #rx""
               #:log-file "./soc.log"
               #:servlet-path ""
	       #:launch-browser? #f)
