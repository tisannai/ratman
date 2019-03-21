;; -*-racket-*-

#lang racket

(module+ test

  (require rackunit)
  (require "../ratman.rkt")
  
  (test-begin

    (define testfile "input/test_file1.txt")
    (define p (ratman-read testfile))

    ;; ------------------------------------------------------------
    ;; Test diff jumping commands and their return values:

    (check-equal? (send p line) 1)
    (check-equal? (send p edited?) #f)
    
    (send p step)
    (check-equal? (send p line) 2)
    (send p step -1)
    (check-equal? (send p line) 1)
    (check-equal? (send (send p step) line) 2)
    (check-equal? (send (send p step -1) line) 1)
    (send p line 5)
    (check-equal? (send p line) 5)
    (send p firstline)
    (check-equal? (send p line) 1)
    (send p lastline)
    (check-equal? (send p line) 10)
    

    ;; ------------------------------------------------------------
    ;; Test searching:

    (check-equal? (send (send p firstline) find #rx"Line 1") #t)
    (check-equal? (send (send p firstline) find #rx"Line 2") #t)
    (check-equal? (send (send p line 4) find #rx"Line 3") #f)
    (check-equal? (send (send (send p firstline) search #rx"Line 1") line) 1)
    (check-equal? (send (send p search #rx"Line 2") line) 2)
    (check-equal? (send (send p search #rx"ne 8") line) 8)
    
    (let ((l (send p excursion (lambda (p)
                                 (send (send p search #rx"Line 9") line)))))
      (check-equal? (send p line) 8)
      (check-equal? l 9))

    (let ((err #f))
      (with-handlers ((ratman-search-error? (lambda (n) (set! err #t) (void))))
        (send p search #rx"Line 2"))
      (check-equal? err #t))
    
    (let ((err #f))
      (with-handlers ((ratman-search-error? (lambda (n) (set! err #t) (void))))
        (send p search #rx"Line 2" #f))
      (check-equal? err #f)
      (check-equal? (send p line) 2))
    
    (let ((err #f))
      (with-handlers ((ratman-search-error? (lambda (n) (set! err #t) (void))))
        (send p search #rx"Line 9" #f))
      (check-equal? err #t)
      (check-equal? (send p line) 2))

    
    ;; ------------------------------------------------------------
    ;; Test content queries:

    (check-equal? (send (send p firstline) get) "Line 1")
    (check-equal? (send p ref 10) "Line 10")
    (check-equal? (send p ref 11) #f)
    (check-equal? (send p line) 1)
    (check-equal? (send p get-range 1 2) #("Line 1" "Line 2"))
    (check-equal? (send p get-for 1 2) #("Line 1" "Line 2"))
    

    ;; ------------------------------------------------------------
    ;; Test content manipulation:
    (let ((l (send p get)))
      (send p set  "foobar")
      (check-equal? (send p get) "foobar")
      (send p set l)
      (check-equal? (send p linecount) 10)

      (send p update (lambda (p c)
                       (regexp-replace #rx"ne" c "en")))
      (check-equal? (send p get) "Lien 1" )
      (send p set l))

    (send p insert "Line -1")
    (check-equal? (send p linecount) 11)
    (check-equal? (send p get) "Line -1")
    (send p delete)
    (check-equal? (send p linecount) 10 )
    (check-equal? (send p get) "Line 1" )

    (let ((c #("foo" "bar")))
      (send p insert c)
      (check-equal? (send p linecount) 12)
      (check-equal? (send p get-range 1 2) c)
      (check-equal? (send (send p delete 2) linecount) 10)

      (check-equal? (send (send p insert-step "foo" 'end) get) "foo")
      (check-equal? (send (send p insert-step "bar" 'after) get) "bar")
      (check-equal? (send p linecount) 12)
      (check-equal? (send p get-range 11 12) c)
      (check-equal? (send p line) (send p linecount))
      (check-equal? (send (send p insert c 'end) linecount) 14)
      (check-equal? (send p get-range 13 14) c)
      (send (send p line 11) delete 4))

    (let ((len (send p linecount)))
      (check-equal? (send (send p insertfile testfile 'first) linecount) (* 2 len))
      (check-equal? (send (send p insertfile testfile 'end) linecount) (* 3 len)))

    (send p do-all
          (lambda (p)
            (send p sub #rx"Line" "foobar")))
    (check-equal? (send p get-range 5 6) #("foobar 5" "foobar 6"))
    (send p blockline)
    (check-equal? (send p line) 30)

    (send p do-range 1 (send p linecount)
          (lambda (p)
            (send p sub "foobar" "Line")))
    (check-equal? (send p get-range 5 6) #("Line 5" "Line 6"))
    (send p blockline)
    (check-equal? (send p linecount) 30)
    (check-equal? (send p line) 30)
    
    (send p do-for 1 (send p linecount)
          (lambda (p)
            (send p sub #rx"Line" "foobar")))
    (check-equal? (send p get-range 5 6) #("foobar 5" "foobar 6"))


    ;; ------------------------------------------------------------
    ;; Test misc commands:
    (let ((org-lines (send p lines)))
      (send p clear)
      (check-equal? (send p lines) #())
      (check-equal? (send p edited?) #t)

      (send p insert org-lines)
      (send p mark)
      (send p step)
      (check-equal? (send p line) 2)
      (send p unmark)
      (check-equal? (send p line) 1))

    ;; ------------------------------------------------------------
    ;; Test file saving:

    (let ((ofile "output/test_file1.txt")
          (r2 #f)
          (r3 #f))
      (send p copy ofile)

      (set! r2 (ratman-read ofile))
      (check-equal? (send p lines) (send r2 lines))

      (set! r3 (ratman-read testfile))
      (send r2 lines (send r3 lines))
      (send r2 write)

      (check-equal? (system (~a "diff " testfile " " ofile)) #t)

      (when (file-exists? ofile)
        (delete-file ofile)))


    ;; ------------------------------------------------------------
    ;; Test Ratman.edit:

    (let ((ofile "output/test_file1.txt"))
      (ratman-edit ofile
                   (lambda (p)
                     (send p insert "foobar")))
      (set! p (ratman-edit ofile))
      (check-equal? (send p get) "foobar")

      (when (file-exists? ofile)
        (delete-file ofile)))

    )
)
