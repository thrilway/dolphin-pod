#lang web-server

(require web-server/http
         web-server/managers/none
         json
         web-server/servlet-env)
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
   ((".well-known" "webfinger") webfinger)))

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

(define (webfinger req)
  (let ((resource (extract-binding/single 'resource (request-bindings req))))
    (get-resource resource)))

(define (get-resource res)
  (response/jsexpr
   (hash 'subject res
         'aliases (list "milway.ca/thrilway" ))))

(serve/servlet start
               #:port 8080
               #:servlet-regexp #rx""
               #:log-file "./soc.log"
               #:servlet-path "")