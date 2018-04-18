# lsh
Lisp Shell

This is a cross-platform shell developped in Racket. To use, either compile to binary using Racket 6.11 or newer, or run:

Windows:
"C:\Program Files\Racket\racket.exe" -f "lsh.rkt" -e "(require 'lsh)" -i

Unix:
racket -f "lsh.rkt" -e "(require 'lsh)" -i

Available commands:

         help    ; displays this message
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
         search  ; equivalent to Google's 'I'm feeling lucky'

Oh, and it evaluates Racket forms from the command line. Remember to (display ) forms if you need to output results to the screen.

Cheers,

Dexter
