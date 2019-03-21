;; -*-racket-*-

#lang racket

(provide ratman%
         ratman-read
         ratman-edit
         ratman-read-file
         ratman-search-error?
         ratman-file-error?)
  
(define dbug
  (lambda args
    (for ((i args))
      (if (string? i)
          (printf i)
          (printf (~a i))))
    (newline)))

(define-struct (ratman-error exn:fail:user) ())
(define-struct (ratman-search-error ratman-error) ())
(define-struct (ratman-file-error ratman-error) ())

(define ratman%
  (class object%
    (super-new)

    (init file.)

    (define file: file.)
    (define lines: #())
    (define line: 0)
    (define mark: #f)
    (define marks: (make-hash))
    (define blockline: #f)
    (define edited: #f)


    (define/public read
      (lambda args
        (let ((filename (default-arg-or-false args file:)))
          (if (file-exists? filename)
              (set! lines: (ratman-read-file filename))
              (ratman-raise make-ratman-file-error
                            (~a "File not found: " filename))))))
    

    (define/public write
      (lambda args
        (let ((filename (default-arg-or-false args file:)))
          (when edited:

            (let ((file-dir (path-only filename)))
              (when (and file-dir
                         (not (file-exists? file-dir)))
                (make-directory* file-dir)))

            (with-output-to-file filename
              (lambda ()
                (for ((line lines:))
                  (if line
                      (fprintf (current-output-port)
                               (~a line "\n"))
                      (fprintf (current-output-port) "\n"))))
              #:exists 'replace)

            (set! edited: #f)))
        this))
    

    (define/public (copy filename)
      (write filename)
      this)


    (define/public line
      (lambda args
        (if (pair? args)
            (begin
              (set! line: (abs-index (car args)))
              this)
            (+ line: 1))))


    (define/public step
      (lambda args
        (let ((dir (if (pair? args) (car args) 1)))
          (set! line: (abs-index (+ (+ line: 1) dir))))
        this))


    (define/public (firstline)
      (set! line: 0)
      this)


    (define/public (lastline)
      (set! line: (- (linecount) 1))
      this)


    (define/public (blockline)
      (when blockline:
        (set! line: blockline:))
      this)


    (define/public lines
      (lambda args
        (if (pair? args)
            (begin
              (set! edited: #t)
              (set! lines: (args-to-vector (car args))))
            lines:)))


    (define/public get
      (lambda args
        (let ((count (default-unsigned-count args 1)))
          (if (= count 1)
              (vector-ref lines: line:)              
              (take-range lines: line: (+ line: (- count 1)))))))


    (define/public ref
      (lambda args
        (if (pair? args)
            (let ((idx (raw-abs-index (car args))))
              (cond
                ((< idx 0) #f)
                ((> idx (- (linecount) 1)) #f)
                (else (vector-ref lines: idx))))
            (vector-ref lines: line:))))
    

    (define/public (set text)
      (set! edited: #t)
      (vector-set! lines: line: text)
      this)
    

    ;; Substitute part of current line content.
    ;;
    ;; args:
    ;;   from     String or regexp.
    ;;   to       Target string.
    (define/public (sub from to)
      (set! edited: #t)
      (vector-set!
       lines:
       line:
       (cond
         ((string? from) (string-replace (vector-ref lines: line:) from to))
         (else (regexp-replace from (vector-ref lines: line:) to))))
      this)
    

    (define/public (update fn)
      (set! edited: #t)
      (vector-set! lines: line: (apply fn (list this (vector-ref lines: line:))))
      this)
    

    ;; Insert lines and move to insertion position.
    ;;
    ;; Position: <num>, 'first, 'after, 'last, 'end.
    ;;
    ;; args:
    ;;   <none>     Insert empty line at current position.
    ;;   text       Insert text at current position.
    ;;   text pos   Insert text at given position and move to it.
    (define/public insert
      (lambda args
        (set! edited: #t)
        (let-values (((index count) (insert-lines args)))
          (set! line: index))
        this))
    

    ;; Insert lines and move to last inserted line.
    ;;
    ;; args:
    ;;   <none>     Insert empty line at current position.
    ;;   text       Insert text at current position.
    ;;   text pos   Insert text at given position (position: <num>, 'first, 'end).
    (define/public insert-step
      (lambda args
        (set! edited: #t)
        (let-values (((index count) (insert-lines args)))
          (line (+ index count)))
        this))


    (define/public delete
      (lambda args
        (set! edited: #t)
        (call/cc
         (lambda (cc)
           (let ((count (default-unsigned-count args 1)))
             (set! lines: (vector-delete lines: line: count)))))
        this))
    

    (define/public (insertfile filename . pos)
      (insert (ratman-read-file filename) (if (pair? pos) (car pos) line:)))
    

    (define/public (insertfile-step filename . pos)
      (insert-step (ratman-read-file filename) (if (pair? pos) (car pos) line:)))


    (define/public (clear)
      (set! edited: #t)
      (set! lines: #())
      (set! line: 0)
      this)


    (define/public (find re-or-str . forward)
      (let ((fwd (if (pair? forward) (car forward) #t)))
        (let ((res (find-or-fail re-or-str fwd)))
          (if res
              (begin
                (set! line: res)
                #t)
              #f))))

    (define/public (search re-or-str . forward)
      (let ((fwd (if (pair? forward) (car forward) #t)))
        (let ((res (find-or-fail re-or-str fwd)))
          (if res
              (begin
                (set! line: res)
                this)
              (ratman-raise make-ratman-search-error
                            (~a "Pattern not found: " re-or-str))))))

    (define/public (linecount)
      (vector-length lines:))


    (define/public (filename)
      file:)
    

    (define/public (edit)
      (set! edited: #t)
      this)
    

    (define/public (edited?)
      edited:)
    

    (define/public (excursion fn)
      (let* ((orgline line:)
             (ret (fn this)))
        (set! line: orgline)
        ret))
    

    (define/public mark
      (lambda args
        (if (pair? args)
            (begin
              (hash-set! marks: (car args) (+ line: 1))
              this)
            (begin
              (set! mark: (+ line: 1))
              this))))
    

    (define/public unmark
      (lambda args
        (if (and (pair? args)
                 (hash-ref marks: (car args)))
            (set! line: (hash-ref marks: (car args)))
            (if mark:
                (begin
                  (set! line: (- mark: 1))
                  (set! mark: #f)
                  this)
                this))))
    

    (define/public (do-all fn)
      (do-range-safe 0 (- (linecount) 1) fn))


    (define/public (do-range start stop fn)
      (let-values (((a b) (normalize-user-indeces start stop)))
        (do-range-safe a b fn)))
    

    (define/public (do-for start count fn)
      (let-values (((a b) (normalize-user-indeces start (+ start (- count 1)))))
        (do-range-safe a b fn)))
    

    (define/public (get-range a. b.)
      (let-values (((a b) (normalize-user-indeces a. b.)))
        (take-range lines: a b)))
    

    (define/public (get-for a. b.)
      (let ((atmp (abs-index a.)))
        (let-values (((a b) (normalize-user-indeces atmp (+ atmp b.))))
          (take-range lines: a b))))
    

    (define/public peek
      (lambda args
        (let ((count (default-unsigned-count args 0))
              (line (+ line: 1)))
          (view-range (- line count) (+ line count) #f))))
    

    (define/public peek-ln
      (lambda args
        (let ((count (default-unsigned-count args 0))
              (line (+ line: 1)))
          (view-range (- line count) (+ line count) #t))))


    (define/public view
      (lambda args
        (let-values (((a b) (view-args-to-range args)))
          (view-range a b #f))))


    (define/public view-ln
      (lambda args
        (let-values (((a b) (view-args-to-range args)))
          (view-range a b #t))))

    

    ;; ------------------------------------------------------------
    ;; Private:

    (define (ratman-raise exc msg)
      (raise (exc
              msg
              (current-continuation-marks))))


    (define (take-range lines a b)
      (vector-drop (vector-take lines (+ b 1)) a))


    (define (args-to-vector args)
      (cond
        ((vector? args) args)
        ((list? args) (list->vector args))
        (else (vector args))))


    (define (vector-insert vec pos items)
      (vector-append (vector-take vec pos)
                     (args-to-vector items)
                     (vector-drop vec pos)))


    (define (vector-delete vec pos count)
      (vector-append (vector-take vec pos)
                     (vector-drop vec (+ pos count))))


    (define (find-or-fail re-or-str forward)
      (let ((line line:)
            (len (linecount)))
        
        (let-values (((off limcmp lim) (if forward
                                           (values + < len)
                                           (values - >= 0))))
          (let ((patcmp (if (string? re-or-str)
                            (lambda (line pat) (string-contains? line pat))
                            (lambda (line pat) (regexp-match? pat line)))))
            
            (call/cc
             (lambda (cc)
               (let loop ((line line))
                 (when (limcmp line lim)
                   (when (and (vector-ref lines: line)
                              (patcmp (vector-ref lines: line) re-or-str))
                     (cc line))
                   (loop (off line 1))))
               #f))))))


    ;; Non-normalized, but absolute, index.
    (define (raw-abs-index a)
      (if (< a 0)
          (+ (linecount) a)
          (- a 1)))


    ;; Normalize index.
    (define (normalize-abs-index a)
      (cond
        ((< a 0) 0)
        ((> a (linecount)) (- (linecount) 1))
        (else a)))


    (define (abs-index a)
      (normalize-abs-index (raw-abs-index a)))


    ;; Return multiple-values of 2.
    (define (normalize-user-indeces a. b.)
      (let ((a (abs-index a.))
            (b (abs-index b.)))
        (when (> a b)
          (set!-values (a b) (values b a)))
        (values a b)))


    (define (do-range-safe a b fn)
      (let ((orgline line:))
        (set! line: a)
        (let loop ((i a))
          (fn this)
          (when (< i b)
            (set! line: (+ line: 1))
            (loop (+ i 1))))
        (set! blockline: line:)
        (set! line: orgline)
        this))


    (define (view-range first last show-lines)
      (let-values (((a b) (normalize-user-indeces first last)))
        (view-range-safe a b show-lines)))


    (define (view-range-safe a b show-lines)
      (let ((lines (take-range lines: a b))
            (lineno (+ a 1)))
        (for ((line lines))
          (if show-lines
              (begin
                (printf (~a (~a lineno #:min-width 3 #:align 'right) ": " line "\n"))
                (set! lineno (+ lineno 1)))
              (printf (~a line "\n"))))))


    (define (default-arg-or-false args default)
      (if (pair? args)
          (car args)
          default))


    (define (default-unsigned-count args default)
      (if (pair? args)
          (if (> (car args) default)
              (car args)
              default)
          default))


    ;; View line content.
    ;;
    ;; * no args:  view all
    ;; * one arg:  view from current onwards by count
    ;; * two args: view given range
    (define (view-args-to-range args)
      (cond
        ((>= (length args) 2) (normalize-user-indeces (first args) (second args)))
        ((= (length args) 1) (normalize-user-indeces (+ line: 1) (+ line: (first args))))
        (else (values 1 (linecount)))))


    (define (pos-to-index pos)
      (cond
        ((number? pos) (abs-index pos))
        ((eq? pos 'first) 0)
        ((eq? pos 'after) (+ line: 1))
        ((eq? pos 'last) (- (linecount) 1))
        ((eq? pos 'end) (linecount))))

    
    (define (insert-lines args)
      (let-values (((text index)
                    (cond
                      ((>= (length args) 2)
                       (values (args-to-vector (first args))
                               (pos-to-index (second args))))
                      ((>= (length args) 1)
                       (values (args-to-vector (first args))
                               line:))
                      (else
                       (values (args-to-vector #f)
                               line:)))))
        ;;(dbug index)
        (if (= index (linecount))
            (set! lines: (vector-append lines: text))
            (set! lines: (vector-insert lines: index text)))
        (values index (vector-length text))))

    ))


;; Create editing session with filename.
(define (ratman-read filename)
  (let ((p (new ratman% (file. filename))))
    (send p read)
    p))


;; Edit file and also create it if it does not exist.
;;
;; If block is given, a file will be opened and block with Ratman
;; commands will be executed for it. Otherwise a Ratman object is
;; returned for further use.
(define (ratman-edit filename . blk)
  (let ((p (new ratman% (file. filename))))
    (when (file-exists? filename)
      (send p read))
    (when (pair? blk)
      ((car blk) p)
      (send p write))
    p))


;; Read file.
;;
;; Empty lines are mapped as false.
(define (ratman-read-file filename)
  (list->vector
   (map
    (lambda (line)
      (if (non-empty-string? line)
          line
          #f))
    (file->lines filename))))
