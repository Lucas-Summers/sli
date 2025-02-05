#lang typed/racket

(require "main.rkt")

(: string->sexp (String -> Sexp))
(define (string->sexp input)
  (with-handlers ([exn:fail? (lambda (e) (error "Invalid AAQZ7 syntax."))])
    (cast (read (open-input-string input)) Sexp)))

(: repl (-> Void))
(define (repl)
  (displayln "Simple Language Interpreter (SLI) REPL. Type 'exit' to quit.")
  (let loop ()
    (display "sli> ")
    (flush-output)
    (define input (read-line))
    (unless (or (eof-object? input) (string-ci=? input "exit"))
      (with-handlers 
          ([exn:fail? (lambda (e) 
                        (displayln (format "Error: ~a" e)))] 
           ;; Catch any error of type `exn:fail?`
           )
        (displayln (top-interp (string->sexp input))))
      (loop))))

(: execute-file (String -> Void))
(define (execute-file filename)
  (with-input-from-file filename
    (lambda ()
      (for-each
       (lambda (line)
         (if (string? line)  ;; Ensure that the line is a string
             (with-handlers
                 ([exn:fail? (lambda (e)
                               (displayln (format "Error: ~a" e)))])  ;; Catch errors
               (let ((sexp (string->sexp line)))  ;; Convert line to Sexp
                 (displayln (top-interp sexp))))  ;; Interpret the Sexp
             (displayln "Warning: Invalid line encountered, skipping.")))  ;; Handle non-strings
       (port->lines (current-input-port))))))

(: main (-> Void))
(define (main)
  (define args (current-command-line-arguments))
  (cond
    [(positive? (vector-length args)) 
     (execute-file (vector-ref args 0))]
    [else (repl)]))

(main)
