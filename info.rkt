#lang info

(define collection "blog")
(define deps '("base"
               "scribble-lib"
               "https://github.com/quasarbright/frog.git"
               "scribble-math"
               "https://github.com/quasarbright/number-diff.git"
               "pict-lib"
               "macro-debugger"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Mike Delmonaco's personal blog")
(define version "0.0")
(define pkg-authors '(mdelmonaco))
(define license '(Apache-2.0 OR MIT))
