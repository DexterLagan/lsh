#lang racket/gui

; A library to explore files and directories DOS and UNIX style.
; Typically, DOS commands will display path objects while their UNIX counterparts will pretty-print paths instead.

; Version history:
; Alpha
; v0.1 - First version. Racket 6.7.
; v0.2 - Adds support for extra commands such as google, touch, edit, find, show, earch and echo. Racket 6.10. 
; v0.3 - Adds support for variables (get/set) and self-editing with edit-me. Racket 6.11.
; v0.3b - Light version without gui library support. Racket 6.11.
; v0.3.1b - fixes save-script. Racket 6.11.
; v0.4 - First posted on GitHub, gui enabled.

(define current-version "v0.3.1b Alpha (nogui)")

(provide help       ; displays this message
         cd      ; displays the current working directory or change it
         cd/     ; same as (cd "/") - goes back to filesystem root
         pwd     ; print the current directory's path
         dir     ; list the current directory's file list or the specified path
         ls      ; prints the current folder's file list
         mkdir   ; makes a folder
         run     ; run a program from the current directory, optionally takes parameters
         run#    ; run a program directly using its path
         racket  ; edit a file using DrRacket
         edit    ; edit a file using notepad
         edit-me ; edit lsh source file using DrRacket
         url     ; browse to an url
         google  ; google an url
         cp      ; copy a file or folder
         mkdir   ; create a folder
         touch   ; create an empty file
         find    ; walk the current path
         show    ; pretty-prints a command result
         rm      ; delete a file
         rmdir   ; delete a folder
         echo    ; display something on the screen
         search) ; equivalent to Google's 'I'm feeling lucky'

;;; defs

;; generic about message
(define (about) (echo "Welcome to LSH (Lisp SHell) " current-version " - type help for usage."))

;; display help page
(define (help)
  (newline)
  (about)
  (displayln "Available commands:
          help             ; displays this message
          cd               ; displays the current working directory or change it
          cd\\              ; same as (cd \"/\") - goes back to the filesystem's root
          pwd              ; print the current directory's path
          dir              ; list the current directory's file list or the specified path
          ls               ; prints the current folder's file list
          run              ; run a program, optionally takes parameters
          racket           ; edit a file using DrRacket
          cat              ; display a text file on the screen
          edit             ; edit a file using notepad
          edit-me          ; edit a file using DrRacket
          url              ; browse to an url
          google           ; google an url
          search           ; search and open first matching link

Advanced commands:
          echo [something]           ; displays something on the screen
          cp src dst                 ; copy a file or folder
          mkdir dst                  ; create a folder
          touch [file path]          ; create an empty file
          find                       ; walk the current path
          find [wildcard]            ; walk and filter the current path
          find pred ...              ; same as find-files
          show v                     ; pretty-prints result of a command that returns a list
          rm file                    ; delete a file
          rmdir dir                  ; delete a folder
          set var = value            ; set a local variable
          set                        ; list local variables and their values
          start-recording            ; start recording a script
          save-script [file path]    ; stops recording and save the script"))

;; generic +
(require (except-in racket +) (rename-in racket [+ old+]))
(define +
  (lambda args
    (cond [(no args) null]
          [(andmap string? args) (apply string-append args)] ; adding strings? string-append!
          [(andmap number? args) (apply old+ args)]          ; adding numbers? add them using the old +
          [(andmap list? args)   (apply append args)]        ; adding list? append!
          [else (apply ~a args)])))                          ; else brute-force ~a arguments :)

;; better displayln/string-append
(define echo
  (λ args
    (displayln (apply ~a args))))

;; all-but-last-element of list
(define (all-but-last l) (reverse (cdr (reverse l))))

;; predicate returns true if current lsh session is being run from inside DrRacket
(define (debug-mode) (string-contains? (path->string (find-system-path 'run-file)) "DrRacket"))

;; Define a global namespace to allow input-loop eval to understand our commands
(define-namespace-anchor a)
(define input-loop-ns (namespace-anchor->namespace a))

;; double-quote command parameters
(define (double-quote-params command-line)
  (let* ((parts (string-split command-line " "))
         (command (first parts))
         (params (rest parts))
         (quoted-params (if (= (length params) 1)
                            (+ "\"" (first params) "\"")
                            (+ "\"" (string-join params " ") "\""))))
    (if (= (length parts) 1) (string-replace command-line "\\" "/")
        (string-replace (string-join (list command quoted-params) " ") "\\" "/"))))

;; Clean up an exception for display
(define (clean-exception e)
  (string-join (cdr (string-split (~a e) " ")) " "))

;; Main evaluation proc
(define (evaluate v)
  (if (non-empty-string? v)
      (with-handlers ([exn:fail:syntax?
                       (λ (e) (displayln (clean-exception e)))]
                      [exn:fail?
                       (λ (e) (displayln (clean-exception e)))])
        (eval (call-with-input-string v read) input-loop-ns))
      (void)))

;; generic invalid command error message
(define (invalid-command command)
  (displayln (+ command " : unknown command or missing parameter. Type 'help' for help.\n")))

;; check that a command is either the exact command alone or the command followed by a space
(define (alone-or-got-param? command-line command)
  (or (string=? command-line command)
      (string-prefix? command-line (+ command " "))))

;; display a nice prompt with the current directory
(define (display-prompt)
  (display (+ (path->string (current-directory)) "> ")))

;; predicate that returns true if given command is built-in
(define (built-in? command)
  (or (string=? command "pwd")
      (string=? command "edit-me")
      (string=? command "start-recording")
      (string-prefix? command "cp ")
      (string-prefix? command "rm ")
      (string-prefix? command "cd ")       ; built-in commands
      (string-prefix? command "cd/")
      (string-prefix? command "cd..")
      (string-prefix? command "cd\\")
      (string-prefix? command "cat ")
      (string-prefix? command "run ")
      (string-prefix? command "url ")
      (string-prefix? command "show ")
      (string-prefix? command "edit ")
      (string-prefix? command "rmdir ")
      (string-prefix? command "touch ")
      (string-prefix? command "mkdir ")
      (alone-or-got-param? command "ls")
      (alone-or-got-param? command "ll")
      (alone-or-got-param? command "dir")
      (alone-or-got-param? command "set")
      (alone-or-got-param? command "find")
      (alone-or-got-param? command "help")
      (alone-or-got-param? command "google")
      (alone-or-got-param? command "save-script")
      (string-prefix? command "search ")
      (string-prefix? command "racket ")))

;; predicate returns true when command is a macro
(define (macro? command)
  (or (alone-or-got-param? command "set")
      (alone-or-got-param? command "get")))

;; multiple non-empty-string? predicate
(define non-empty-strings?
  (λ args
    (andmap non-empty-string? args)))

;; local variable list
(define local-vars null)

;; adds a variable to the local variable list
(define (add-local-var var)
  (set! local-vars (cons var local-vars)))

;; predicate returns true if variable is in the local variable list
(define (local-var? var)
  (member var local-vars))

;; pretty prints variables set through the 'SET' command
(define (display-local-vars)
  (if (null? local-vars) (displayln "No local variable set.")
      (for-each (λ (var) (displayln (+ var " = " (evaluate var)))) local-vars)))                ; for each variable in the list, display   varable-name = variable-value

;; get macro
(define (matches-get command params)
  (if (and (string=? command "get")
           (null? params)) (display-local-vars)
                           #f))

;; set macro
(define (matches-set command params p1 p2 p3)
  (cond ((and (string=? command "set")
              (string=? p2 "=")
              (non-empty-strings? p1 p3)) (add-local-var p1)      ; save variable in local variable list
                                          (+ "(define " p1 " " p3 ")"))
        ((and (string=? command "set")
              (null? params)) (display-local-vars) "(void)")
        (else #f)))

;; syntax rules
(define (matches-syntax-rules command params p1 p2 p3)
  (or (matches-get command params)
      (matches-set command params p1 p2 p3)))

;; transform syntax to match racket's
(define (transform-syntax stx)
  (if (non-empty-string? stx)
      (let* ((parts      (string-split stx " "))                               ; split syntax parts
             (count      (length parts))                                       ; get syntax part count
             (command    (car parts))                                          ; first syntax part
             (params     (cdr parts))                                          ; rest of syntax parts
             (p1     (if (> count 1) (cadr parts) ""))                         ; second syntax part
             (p2     (if (> count 2) (caddr parts) ""))                        ; third syntax part
             (p3     (if (> count 3) (cadddr parts) ""))                       ; fourth syntax part
             (others (if (> count 4) (cdddr parts) "")))                       ; rest of the syntax parts
        (unless (matches-syntax-rules command params p1 p2 p3)
          (+ "(echo \"Macro transformer error: I don't understand '" (~a parts) "'.
                              command: " (~a command) "
                              param1: "  (~a p1) "
                              param2: "  (~a p2) "
                              param3: "  (~a p3) "
                              param-rest: "  (~a others) "\")")))
      ""))                                                                     ; return empty string as default

;; handle built-in commands
(define (handle-built-in command)
  (begin
    (define transformed-command (if (macro? command) (transform-syntax command)                ; if command is a macro, transform syntax,
                                    (double-quote-params command)))                            ; else automatically double-quote parameters
    ;(echo "Transformed command: '" (~a transformed-command) "'")                              ; for debugging
    (define final-command (cond ((string=? transformed-command "") "")                         ; return nothing if transformed command is empty
                                ((string-prefix? transformed-command "find")                   ; if this is one of the commands returning a list - like find
                                 (+ "(show (" transformed-command "))"))                       ; automatically use show to pretty-print list
                                ((string-prefix? transformed-command "(") transformed-command) ; if the command is already an s-expression, return it 
                                (else (+ "(" transformed-command ")"))))                       ; else transform into s-expression
    ;(echo "About to execute: '" (~a final-command) "'")                                       ; for debugging
    (if (non-empty-strings? transformed-command final-command)                                 ; make sure transformed and final syntaxes are not empty
        (evaluate final-command) (invalid-command command))                                    ; Evaluate final command
    (newline)))                                  

;; clean up entry from line-feeds and carriage returns
(define (clean-up command)
  (string-trim (string-replace2 command "\n" "\r" "" "")))


;; currently recording a script?
(define recording-script? #f)
(define current-script null)

;; adds a line to the script currently being recorded
(define (record-script line)
  (set! current-script (cons line current-script)))

;; generic confirmation line
(define (show-confirmation-line msg)
  (display (+ msg " "))
  (let ((answer (read-line)))
    (if (string-prefix? answer "y") #t #f)))

;; start recording a script
(define (start-recording)
  (set! current-script null)
  (set! recording-script? #t))

;; save the current script to file and replace existing
(define (save-script file)
  (let ((write-file (λ () (begin
                            (with-handlers ([exn:fail?
                                             (λ (e) (displayln "Access denied writing file. Try again."))])
                              (display-lines-to-file (all-but-last (reverse current-script)) file #:exists 'replace #:separator #"\r"))
                            (set! recording-script? #f)))))
    (if (file-exists? file)
        (when (show-confirmation-line "File exists. Overwrite?") 
          (write-file))
        (write-file))))

;; Input loop
(define (input-loop)
  (let/ec break
    (let loop ()
      (display-prompt)                                                                         ; display command prompt
      (define command (clean-up (read-line)))                                                  ; get input from user; trim and remove annoying enters and returns
      (when recording-script? (record-script command))
      (cond [(string=? command "") (loop)]                                                     ; if nothing entered, loop  
            [(string-prefix? command "exit") (break)]                                          ; the only thing that breaks the loop apart from ctrl-c
            [(built-in? command) (handle-built-in command)]                                    ; detect built-in commands and handle them
            [(local-var? command) (displayln (evaluate command))]                              ; if the command is recognized as a local variable, display its value
            [(string-prefix? command "(") (begin (evaluate command) (newline))]                ; Evaluate s-expressions directly
            [(file-exists? command) (run# command)]                                            ; if the file specified exists, run it
            [(cond ((file-exists? (+ command ".exe")) (run# (+ command ".exe")))               ; else,
                   ((file-exists? (+ command ".com")) (run# (+ command ".com")))               ; if a similar file with an executable extension is found,
                   ((file-exists? (+ command ".bat")) (run# (+ command ".bat")))               ; run
                   ((file-exists? (+ command ".sh"))  (run# (+ command ".sh"))))]              ; it.
            [else (invalid-command command)])
      (loop)))
  (displayln "Goodbye!"))

;; macros

;; a cosmetic macro -- adds then, else
(define-syntax my-if             ; macro name
  (syntax-rules (then else)      ; literals it uses, if any
    ((my-if e1 then e2 else e3)  ; pattern
     (if e1 e2 e3))))            ; template

;; shortcuts
(define no null?)
(define mkdir make-directory)
(define cp copy-directory/files)
(define rm delete-file)
(define rmdir delete-directory)
(define (show v)
  (for-each displayln v))

;; double string-replace
(define (string-replace2 s p1 p2 r1 r2)
  (string-replace (string-replace s p1 r1) p2 r2))

;; displays current directory or changes it - supports 'cd ~' and 'cd ~/Downloads'
(define cd
  (λ args
    (cond ((no args) (current-directory))
          (else (let ((fa (first args)))
                  (cond ((string? fa) (cond ((string=? fa "~") (current-directory (find-system-path 'home-dir)))
                                            ((string-prefix? fa "~/") (current-directory (+ (path->string (find-system-path 'home-dir)) (string-replace fa "~/" ""))))
                                            (else (current-directory fa))))
                        (else (current-directory args))))))))

(define (cd/)  (cd "/"))
(define (cd\\) (cd "\\"))
(define (cd..) (cd ".."))

;; create an empty file
(define (touch file)
  (display-to-file "" file))

;; prints the current working directory path as a string
(define (pwd)
  (displayln (path->string (current-directory))))

;; lists the files in the current directory
(define dir directory-list)

;; dir
(define (ll)
  (display (string-replace
            (with-output-to-string (λ () (system (cond ((equal? (system-type 'os) 'windows) "dir")
                                                       ((equal? (system-type 'os) 'unix)    "ls -la")
                                                       ((equal? (system-type 'os) 'macosx)  "ls -la")
                                                       (else "ls -la")))))
            "\r" "")))

;; unix cat equivalent
(define (cat file)
  (if (> (file-size file) 64000) (displayln "File too big for display.")
      (let ([in (open-input-file file #:mode 'text)])
        (displayln (read-string (file-size file) in)))))

;; prints the current working directory listing as a string
(define (ls)
  (for-each displayln (map path->string (directory-list))))

;; prints the current working directory listing as a string with file sizes and types
(define (ll#)
  (let* ((paths (directory-list))
         (listing (map path->string paths))
         (sizes (map number->string (map file-size paths)))
         (files-and-sizes (map
                           (λ (s1 s2) (+ s1 "      " s2))
                           listing sizes)))
    (for-each displayln files-and-sizes)))

;; run a program from the current directory
(define (run program [params ""])
  (void (shell-execute #f (+ (path->string (current-directory)) program) params
                       (current-directory) 'sw_shownormal)))

;; run a program from the path supplied and detects the absolute path
(define (run# program [params ""])
  (void (shell-execute #f (if (or (string-contains? program "/")
                                  (string-contains? program "\\")) program (+ (path->string (current-directory)) program))
                       params
                       (current-directory) 'sw_shownormal)))

;; edit a file using DrRacket
(define (racket file)
  (void (shell-execute "open" file ""
                       (current-directory) 'sw_shownormal)))

;; edit the present lsh source file using DrRacket
(define (edit-me)
  (void (shell-execute "open" (+ (current-directory-for-user) "lsh.rkt") ""
                       (current-directory-for-user) 'sw_shownormal)))

;; edit a file using notepad
(define (edit file)
  (void (shell-execute "open" "notepad" file
                       (current-directory) 'sw_shownormal)))

;; open an URL
(define (url url)
  (void (shell-execute #f (+ "http://" url) ""
                       (current-directory) 'sw_shownormal)))

;; google something
(define (google something)
  (void (shell-execute #f (+ "https://www.google.com/search?q=" (string-replace something " " "+")) ""
                       (current-directory) 'sw_shownormal)))

;; google I'm feeling lucky
(define (search something)
  (void (shell-execute #f (+ "https://www.google.com/search?q=" (string-replace something " " "+") "&btnI") ""
                       (current-directory) 'sw_shownormal)))

;; better find-files
(define (any file) #t)
(define (all-but-first-two  s) (list->string (cddr                    (string->list s))))
(define (all-but-last-two   s) (list->string (reverse (cddr  (reverse (string->list s))))))
(define (all-but-last-three s) (list->string (reverse (cdddr (reverse (string->list s))))))
(define begins-with? string-prefix?)
(define ends-with? string-suffix?)

(define find
  (λ args
    (cond ((no args) (find-files any))
          (else (let ((fa (first args)))
                  (cond ((string? fa) (cond ((begins-with? fa "*.")  (find-files (λ (path) (ends-with?   (path->string path) (+ "." (all-but-first-two  fa))))))
                                            ((ends-with?   fa "*.*") (find-files (λ (path) (begins-with? (path->string path)        (all-but-last-three fa)))))
                                            ((ends-with?   fa ".*")  (find-files (λ (path) (begins-with? (path->string path) (+     (all-but-last-two   fa) ".")))))
                                            (else (find-files (λ (s) (string=? s fa))))))
                        (else (find-files args))))))))

;;; main
(about)
(newline)
(when (not (debug-mode)) (input-loop))
(input-loop)



