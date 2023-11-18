#lang racket/base
(require db racket/contract)
(struct/contract pod ((db connection?)))
(struct/contract pod-tag ((pod pod?) (id integer?)))
(struct/contract pod-podcast ((pod pod?) (id number?)))
(struct/contract pod-author ((pod pod?) (id integer?)))
(struct/contract pod-episode ((pod pod?) (id integer?)))
(struct/contract pod-user ((pod pod?) (id number?)))
(struct/contract pod-profile ((pod pod?) (id number?)))
(struct/contract pod-review ((pod pod?) (id number?)))
(struct/contract pod-comment ((pod pod?) (id integer?)))

(provide (all-defined-out))