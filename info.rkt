#lang info
(define collection "ratman")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/ratman.scrbl" ())))
(define pkg-desc "Ratman is a Racket version of Ruby Patman library.")
(define version "0.1")
(define pkg-authors '(Tero Isannainen))
(define compile-omit-paths '("test"))
