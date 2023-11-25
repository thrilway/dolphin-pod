#lang racket

(require web-server/servlet
         web-server/formlets
         web-server/http/id-cookie
         web-server/templates
         json xml xml/path
         ;"webfinger.rkt"
         "model.rkt"
         "workers.rkt"
         "session.rkt"
         "join.rkt")
(define the-pod (init-pod!))
(define-values (dispatch url-generator)
  (dispatch-rules
   (("") home)
   (("welcome") welcome)
   (("join") join)
   (("join") #:method "post" join-post)
   (("login") login)
   (("login") #:method "post" login-post)
   (("verify-join" (string-arg)) verify-join)
   (("import") import-page)
   (("import") #:method "post" import-post)
   (("subscribe") subscribe)
   (("subscribe") #:method "post" subscribe-post)
   (("episode" (integer-arg)) episode-page)))

(define/contract (not-implemented url-str)
  (-> string? response?)
  (define message/bytes (string->bytes/utf-8 (format "~s Not Implemented" url-str)))
  (response/empty #:code 502
                 #:message message/bytes))
;Handlers
(define/contract (home req)
  (-> request? response?)
  (define user-or-false (get-session-user req))
  (if user-or-false
      (render-home-page user-or-false)
      (redirect-to (url-generator welcome))))
(define/contract (welcome req)
  (-> request? response?)
  (render-welcome-page the-pod))
(define/contract (join req)
  (-> request? response?)
  (render-join-page the-pod))

(define/contract (join-post req)
  (-> request? response?)
  (let-values (((username email password) (formlet-process join-formlet req)))
    (begin
      (start-signup! the-pod username email password)
      (render-join-started-page))))
(define/contract (verify-join req ver-str)
  (-> request? string? response?)
  (define user-or-false (finish-signup! the-pod ver-str))
  (if user-or-false
      (let ((c (make-session/cookie! user-or-false)))
        (redirect-to (url-generator home) #:headers (list (cookie->header c))))
      (render-failed-verify)))

(define/contract (login req)
   (-> request? response?)
  (let ((referer-assoc (assoc 'referer (request-headers req))))
    (if (and (pair? referer-assoc)
             (not (string=? (cdr referer-assoc) (url-generator welcome))))
        (render-login-page the-pod (cdr referer-assoc))
        (render-login-page the-pod "/"))))

(define/contract (login-post req)
   (-> request? response?)
  (let-values (((username-or-email password) (formlet-process login-formlet req))
               ((referer) (if (assoc 'referer (request-headers req)) (cdr (assoc 'referer (request-headers req))) "/")))
    (let ((user-or-false (pod-user-login the-pod username-or-email password)))
      (if user-or-false
          (let ((c (make-session/cookie! user-or-false)))
            (redirect-to referer #:headers (list (cookie->header c))))
          (response/empty #:code 401)))))
(define/contract (import-page req)
  (-> request? response?)
  (response/empty #:code 501))
(define/contract (import-post req)
  (-> request? response?)
  (response/empty #:code 501))
(define/contract (subscribe req)
  (-> request? response?)
  (define user-or-false (get-session-user req))
  (if (not user-or-false)
      (redirect-to (url-generator login))
      (let* ((the-headers (request-headers req))
             (referer (if (assoc 'referer the-headers)
                          (cdr (assoc 'referer the-headers))
                          "/")))
        (render-subscribe-page user-or-false referer))))
(define/contract (subscribe-post req)
  (-> request? response?)
  (define user-or-false (get-session-user req))
  (unless user-or-false
    (response/empty #:code 500))
  (define feed-url (formlet-process subscribe-formlet req))
  (define the-profile (pod-user-profile user-or-false))
  (define the-podcast (pod-insert-podcast! the-pod feed-url))
  (pod-subscribe! the-profile the-podcast)
  (let* ((the-headers (request-headers req))
         (referer (if (assoc 'referer the-headers)
                      (cdr (assoc 'referer the-headers))
                      "/")))
    (redirect-to referer)))
(define/contract (episode-page req ep-id)
  (-> request? integer? response?)
  (define user-or-false (get-session-user req))
  (define the-ep (pod-episode the-pod ep-id))
  (if (pod-episode-exists? the-ep)
      (render-episode-page the-ep user-or-false)
      (response/empty #:code 404)))

;formlets
(define join-formlet
  (formlet
   (#%#
    (div (label ((for "username")) "User name")
         ,{(to-string (required (text-input #:attributes (list (list 'id "username"))))) . => . username})
    (div (label ((for "email")) "Email")
         ,{(to-string (required (text-input #:attributes (list (list 'id "email"))))) . => . email})
    (div (label ((for "password")) "Password")
         ,{(to-string (required (password-input #:attributes (list (list 'id "password"))))) . => . password}))
   (values username email password)))
(define login-formlet
  (formlet
   (#%#
    (div (label ((for "userid")) "User name or Email")
         ,{(to-string (required (text-input #:attributes (list (list 'id "userid"))))) . => . user-id})
    (div (label ((for "password")) "Password")
         ,{(to-string (required (password-input #:attributes (list (list 'id "password"))))) . => . password}))
   (values user-id password)))
(define subscribe-formlet
  (formlet
   (div ((id "feedUrlDiv")) (label ((for "feedUrl")) "Feed URL")
         ,{(to-string (required (text-input #:attributes (list (list 'id "feedUrl"))))) . => . feed-url})
   feed-url))
;Renderers
(define/contract (render-login-page a-pod redir-uri)
  (-> pod? string? response?)
  (define title "Dolphin — Login")
  (define body (include-template "view/login.html"))
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))
(define/contract (render-welcome-page a-pod)
  (-> pod? response?)
  (define title "Dolphin Pod — Welcome")
  (define body (include-template "view/welcome.html"))
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))
(define/contract (render-home-page a-user)
  (-> pod-user? response?)
  (define a-profile (pod-user-profile a-user))
  (define title "Dolphin Pod — Home")
  (define body (include-template "view/home.html"))
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))
(define/contract (render-join-page a-pod)
  (-> pod? response?)
  (define title "Dolphin Pod — Sign up")
  (define body (include-template "view/signup-form.html"))
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))
(define/contract (render-join-started-page)
  (-> response?)
  (define title "Dolphin Pod — Verification Email Sent")
  (define body "Check Your email")
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))
(define/contract (render-failed-verify)
  (-> response?)
  (define title "Dolphin Pod — Invalid Verification URL")
  (define body "Invalid URL")
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))
(define/contract (render-subscribe-page a-user redir-uri)
  (-> pod-user? string? response?)
  (define title "Dolphin — Subscribe")
  (define body (include-template "view/subscribe.html"))
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))
(define/contract (render-episode-page an-episode a-user)
  (-> pod-episode? (or/c #f pod-user?) response?)
  (define the-profile (if (pod-user? a-user)
                          (pod-user-profile a-user)
                          #f))
  (define title (format "Dolphin — ~a" (pod-episode-title an-episode)))
  (define body (include-template "view/episode.html"))
  (response/full
   200 #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (map string->bytes/utf-8 (string-split
                             (include-template "view/wrapper.html")
                             "\n"))))

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
               #:servlet-path ""
               #:ssl? #t
               #:ssl-cert "localhost.crt"
               #:ssl-key "localhost.key")
