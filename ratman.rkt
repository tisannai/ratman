;; -*-racket-*-

#lang racket

(provide ratman%
         ratman-read
         ratman-edit
         ratman-read-file
         ratman-search-error?
         ratman-file-error?
         ratman-call-method
         ratman-read-with
         )

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


    ;; Read file in.
    (define/public read
      (lambda args
        (let ((filename (apply-arg-or-default args file:)))
          (if (file-exists? filename)
              (set! lines: (ratman-read-file filename))
              (ratman-raise make-ratman-file-error
                            (~a "File not found: " filename))))))


    ;; Write Ratman content to disk.
    (define/public write
      (lambda args
        (let ((filename (apply-arg-or-default args file:)))
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


    ;; Copy Ratman content to file.
    (define/public (copy filename)
      (write filename)
      this)


    ;; Return or set line.
    (define/public line
      (lambda args
        (if (pair? args)
            (begin
              (set! line: (abs-index (car args)))
              this)
            (+ line: 1))))


    ;; Step forward or backward current position.
    (define/public step
      (lambda args
        (let ((dir (if (pair? args) (car args) 1)))
          (set! line: (abs-index (+ (+ line: 1) dir))))
        this))


    ;; Jump to first line.
    (define/public (firstline)
      (set! line: 0)
      this)


    ;; Jump to last line.
    (define/public (lastline)
      (set! line: (- (linecount) 1))
      this)


    ;; Jump to line after block.
    (define/public (blockline)
      (when blockline:
        (set! line: blockline:))
      this)


    ;; Get or set all Ratman content.
    (define/public lines
      (lambda args
        (if (pair? args)
            (begin
              (set! edited: #t)
              (set! lines: (args-to-vector (car args))))
            lines:)))


    ;; Get current line or lines by count.
    (define/public get
      (lambda args
        (let ((count (default-unsigned-count args 1)))
          (if (= count 1)
              (vector-ref lines: line:)
              (take-range lines: line: (+ line: (- count 1)))))))


    ;; Get current line or any line.
    (define/public ref
      (lambda args
        (if (pair? args)
            (let ((idx (raw-abs-index (car args))))
              (cond
                ((< idx 0) #f)
                ((> idx (- (linecount) 1)) #f)
                (else (vector-ref lines: idx))))
            (vector-ref lines: line:))))


    ;; Set current line.
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


    ;; Update current line content (i.e. get&set) with the return value
    ;; of the given block. Hence last stmt should include the new line
    ;; content.
    ;;
    ;; Example:
    ;;
    ;;     (send p update
    ;;           (lambda (p c)
    ;;             (regexp-replace #rx"foo" c "bar")))
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


    ;; Delete current line.
    (define/public delete
      (lambda args
        (set! edited: #t)
        (call/cc
         (lambda (cc)
           (let ((count (default-unsigned-count args 1)))
             (set! lines: (vector-delete lines: line: count)))))
        this))


    ;; Insert file to current position.
    (define/public (insertfile filename . pos)
      (insert (ratman-read-file filename) (if (pair? pos) (car pos) line:)))


    ;; Insert file to current position and step.
    (define/public (insertfile-step filename . pos)
      (insert-step (ratman-read-file filename) (if (pair? pos) (car pos) line:)))


    ;; Clear Ratman content and reset current line.
    (define/public (clear)
      (set! edited: #t)
      (set! lines: #())
      (set! line: 0)
      this)


    ;; Find Regexp or literal string forwards or backwards. Return true
    ;; on success.
    (define/public (find re-or-str . forward)
      (let ((fwd (if (pair? forward) (car forward) #t)))
        (let ((res (find-or-fail re-or-str fwd)))
          (if res
              (begin
                (set! line: res)
                #t)
              #f))))


    ;; Search Regexp or literal string forwards or backwards. Fail with
    ;; expection (ratman-search-error) if not found.
    (define/public (search re-or-str . forward)
      (let ((fwd (if (pair? forward) (car forward) #t)))
        (let ((res (find-or-fail re-or-str fwd)))
          (if res
              (begin
                (set! line: res)
                this)
              (ratman-raise make-ratman-search-error
                            (~a "Pattern not found: " re-or-str))))))


    ;; Return line count of Ratman content.
    (define/public (linecount)
      (vector-length lines:))


    ;; Return Ratman file name.
    (define/public (filename)
      file:)


    ;; Mark content modified (explicit).
    (define/public (edit)
      (set! edited: #t)
      this)


    ;; Return true if content is modified.
    (define/public (edited?)
      edited:)


    ;; Execute block, retain current position, and return block value.
    (define/public (excursion fn)
      (let* ((orgline line:)
             (ret (fn this)))
        (set! line: orgline)
        ret))


    ;; Mark (store) current position to default or to named mark.
    (define/public mark
      (lambda args
        (if (pair? args)
            (begin
              (hash-set! marks: (car args) (+ line: 1))
              this)
            (begin
              (set! mark: (+ line: 1))
              this))))


    ;; Unmark (restore) current position from default or from named
    ;; mark.
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


    ;; Execute given block for all lines, i.e. all positions. Block
    ;; parameter is Ratman.
    (define/public (do-all fn)
      (do-range-safe 0 (- (linecount) 1) fn))


    ;; Execute given block between start and stop positions, and update
    ;; position.
    (define/public (do-range start stop fn)
      (let-values (((a b) (normalize-user-indeces start stop)))
        (do-range-safe a b fn)))


    ;; Execute given block starting from start by count, and update
    ;; position.
    (define/public (do-for start count fn)
      (let-values (((a b) (normalize-user-indeces start (+ start (- count 1)))))
        (do-range-safe a b fn)))


    ;; Get lines between start and stop positions inclusive.
    (define/public (get-range a. b.)
      (let-values (((a b) (normalize-user-indeces a. b.)))
        (take-range lines: a b)))


    ;; Get lines starting from start by count.
    (define/public (get-for a. b.)
      (let ((atmp (abs-index a.)))
        (let-values (((a b) (normalize-user-indeces atmp (+ atmp b.))))
          (take-range lines: a b))))


    ;; View line content around current position (by count).
    (define/public peek
      (lambda args
        (let ((count (default-unsigned-count args 0))
              (line (+ line: 1)))
          (view-range (- line count) (+ line count) #f))))


    ;; View line content with line numbers around current position (by
    ;; count).
    (define/public peek-ln
      (lambda args
        (let ((count (default-unsigned-count args 0))
              (line (+ line: 1)))
          (view-range (- line count) (+ line count) #t))))


    ;; View line content.
    ;;
    ;; * no args:  view all
    ;; * one arg:  view from current onwards by count
    ;; * two args: view given range
    (define/public view
      (lambda args
        (let-values (((a b) (view-args-to-range args)))
          (view-range a b #f))))


    ;; View line content with line numbers.
    ;;
    ;; * no args:  view all
    ;; * one arg:  view from current onwards by count
    ;; * two args: view given range
    (define/public view-ln
      (lambda args
        (let-values (((a b) (view-args-to-range args)))
          (view-range a b #t))))



    ;; ------------------------------------------------------------
    ;; Private:

    ;; Raise ratman exception.
    (define (ratman-raise exc msg)
      (raise (exc
              msg
              (current-continuation-marks))))


    ;; Take a range of lines.
    (define (take-range lines a b)
      (vector-drop (vector-take lines (+ b 1)) a))


    ;; Convert arguments to vector.
    ;;
    ;; Do nothing if already a vector.
    (define (args-to-vector args)
      (cond
        ((vector? args) args)
        ((list? args) (list->vector args))
        (else (vector args))))


    ;; Insert to vector.
    (define (vector-insert vec pos items)
      (vector-append (vector-take vec pos)
                     (args-to-vector items)
                     (vector-drop vec pos)))


    ;; Delete from vector.
    (define (vector-delete vec pos count)
      (vector-append (vector-take vec pos)
                     (vector-drop vec (+ pos count))))


    ;; Find re-or-str (or fail) to given direction.
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


    ;; Return absolute index.
    (define (abs-index a)
      (normalize-abs-index (raw-abs-index a)))


    ;; Return multiple-values of 2.
    (define (normalize-user-indeces a. b.)
      (let ((a (abs-index a.))
            (b (abs-index b.)))
        (when (> a b)
          (set!-values (a b) (values b a)))
        (values a b)))


    ;; Safe execution of range.
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


    ;; View range of lines.
    (define (view-range first last show-lines)
      (let-values (((a b) (normalize-user-indeces first last)))
        (view-range-safe a b show-lines)))


    ;; View range of lines safely.
    (define (view-range-safe a b show-lines)
      (let ((lines (take-range lines: a b))
            (lineno (+ a 1)))
        (for ((line lines))
          (if show-lines
              (begin
                (printf (~a (~a lineno #:min-width 3 #:align 'right) ": " line "\n"))
                (set! lineno (+ lineno 1)))
              (printf (~a line "\n"))))))


    ;; Apply first from args or default.
    (define (apply-arg-or-default args default)
      (if (pair? args)
          (car args)
          default))


    ;; Return given count of default.
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


    ;; Convert position to index.
    (define (pos-to-index pos)
      (cond
        ((number? pos) (abs-index pos))
        ((eq? pos 'first) 0)
        ((eq? pos 'after) (+ line: 1))
        ((eq? pos 'last) (- (linecount) 1))
        ((eq? pos 'end) (linecount))))


    ;; Insert lines.
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

    )) ;; ratman%



;; ------------------------------------------------------------
;; User interface functions:

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



;; ------------------------------------------------------------
;; User interface macros:

(begin-for-syntax
  (require racket))

;; Transform ratman-obj reference to method call.
;;
;; call: (ratman-call-method obj <args>)
;;
;; out: (send/apply obj (list <args>))
;;
(define-syntax (ratman-call-method stx)
  (define parts (syntax->datum stx))
  (datum->syntax stx `(send/apply ratman-obj ,(cadr parts) (list ,@(cddr parts)))))


;; Transform to Ratman read access and to related actions.
;;
;;                         caadr        cddr
;; call: (ratman-read-with (testfile r) (displayln (r line)))
;;
;; out:
;;     (let ((ratman-obj (ratman-read testfile)))
;;       (define-syntax r (make-rename-transformer #'ratman-call-method))
;;       (displayln (r line)))
(define-syntax (ratman-read-with stx)
  (define s (syntax->datum stx))
  (datum->syntax
   stx
   (if (> (length (cadr s)) 1)
       `(let ((ratman-obj (ratman-read ,(caadr s))))
          (define-syntax ,(cadadr s) (make-rename-transformer (syntax ratman-call-method)))
          ,@(cddr s))
       `(let ((ratman-obj (ratman-read ,(caadr s))))
          (define-syntax r (make-rename-transformer (syntax ratman-call-method)))
          ,@(cddr s)))))
