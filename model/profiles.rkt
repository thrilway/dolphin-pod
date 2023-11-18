#lang racket/base

(require db
         "../model.rkt")

(define/contract (pod-init-profiles! a-pod)
  (-> pod? void)
  (query-exec (pod-db a-pod)
              (string-append
               "CREATE TABLE profiles ("
               "id serial NOT NULL, "
               "name text, "
               "icon_id integer REFERENCES images (id), "
               "image_id integer REFERENCES images (id), "
               "description text, "
               "CONSTRAINT profiles_key PRIMARY KEY (id))")))