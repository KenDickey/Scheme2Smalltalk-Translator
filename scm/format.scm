; FILE:         "format.scm"
; IMPLEMENTS:   Format function {Scheme} -- see documentation below.
; AUTHOR:       Ken Dickey
; DATE:         1988
; LAST UPDATE:  1993 September 13 -- do without string ports
;               1992 January 8 -- now implements ~& option
;               1991 November 25 -- now uses string ports
; NOTES:       Imports PRETTY-PRINT (~g) and OBJECT->STRING
;              Pretty print and various other code is available via ftp
;              from the Scheme Repository on nexus.yorku.ca [130.63.9.1] 
;              under pub/scheme.  Contact: Ozan Yigit: oz@nexus.yorku.ca.
;
;;  ========
;;  FUNCTION: (FORMAT <port> <format-string> . <args>)
;;  ========
;;  RESULT: returns unconsumed <args> or a string; has side effect of
;;  printing according to <format-string>.  If <port> is #t the output is
;;  to the current output port.  If <port> is #f, a formatted string is
;;  returned as the result of the call.  Otherwise <port> must be an
;;  output port.  <format-string> must be a string.  Characters are output
;;  as if the string were output by the DISPLAY function with the
;;  exception of those prefixed by a tilde (~) as follows [note that options
;;  which take arguments remove them from the argument list (they are said to
;;  be `consumed')]:
;;
;;option  mnemonic: description
;;------  ------------------------
;;   ~a  any: display the argument (as for humans).
;;   ~s  slashified: write the argument (as for parsers).
;;   ~d  decimal: the integer argument is output in decimal format.
;;   ~x  hexadecimal: the integer argument is output in hexadecimal format.
;;   ~o  octal: the integer argument is output in octal format.
;;   ~b  binary: the integer argument is output in binary format.
;;   ~p  plural: if the argument is > than 1, a lower case 's' is printed.
;;   ~c  character: the next argument is displayed as a character.
;;   ~_  space: output a space character.
;;   ~%  newline: output a newline character.
;;   ~&  freshline: unless at the beginning of a line, same as ~%, else ignored
;;   ~~  tilde: output a tilde.
;;   ~t  tab: output a tab charcter. **implemented, but system dependent**
;;   ~g  glorify: pretty print the argument (typically an s-expression).
;;   ~|  page seperator: output a page seperator.
;;   ~?  indirection: take the next argument as a format string and consume
;;       further arguments as appropriate, then continue to process the current
;;       format string.
;;

;----- IMPLEMENTATION SPECIFIC OPTIMIZATIONS
;; (##declare (standard-bindings) (fixnum)) ;; GAMBIT (v1.71)

;---------- FORMAT

(define FORMAT 
  (let ( (LAST-WAS-NEWLINE #f)  
         ; state shared between invocations      
         (ASCII-TAB   (integer->char  9))        
         (ASCII-FF    (integer->char 12))       
         (DONT-PRINT  (string->symbol ""))  ;; a zero character symbol
       ) 
    
    (lambda (<output-port> <format-string> . <args>)  
      
      (letrec ( (PORT 
                 (cond ((output-port? <output-port>) <output-port>)
                       ((eq? <output-port> #t) (current-output-port)) 
                       ((eq? <output-port> #f) (open-output-string)) 
                       (else (error "format: bad port -> " <output-port>))
                ) )
                (RETURN-VALUE 
                 (if (eq? <output-port> #f)  ;; format to a string 
                   (lambda () (get-output-string port)) 
                   (lambda () dont-print)) 
                 )        
             )    
        
        (define (FORMAT-HELP format-strg arglyst)
          (letrec ( 
             (LENGTH-OF-FORMAT-STRING (string-length format-strg))
             
             (ANYCHAR-DISPATCH       
              (lambda (pos arglist last-char-was-nl) 
                (if (>= pos length-of-format-string) 
                  (begin                    
                    (set! last-was-newline last-char-was-nl) 
                    arglist ; used for ~? continuance 
                  )              
                  (let ( (char (string-ref format-strg pos)) ) 
                    (cond            
                     ((eq? char #\~)   
                      (tilde-dispatch (+ pos 1) arglist last-char-was-nl)) 
                     (else                   
                      (write-char char port)     
                      (anychar-dispatch (+ pos 1) arglist #f)        
                      ))               
                    ))        
                )) ; end anychar-dispatch   
             
             (TILDE-DISPATCH          
              (lambda (pos arglist last-char-was-nl)          
                (cond           
                 ((>= pos length-of-format-string)   
                  (write-char #\~ port) ; tilde at end of string is just output
                  (set! last-was-newline last-char-was-nl)
                  arglist ; used for ~? continuance
                  )     
                 (else      
                  (case (char-upcase (string-ref format-strg pos)) 
                    ((#\A)       ; Any -- for humans 
                     (display (car arglist) port)    
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f) 
                     )   
                    ((#\S)       ; Slashified -- for parsers  
                     (write (car arglist) port)     
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f) 
                     )      
                    ((#\D)       ; Decimal
                     (display (number->string (car arglist) 10) port)  
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)  
                     )            
                    ((#\X)       ; Hexadecimal    
                     (display (number->string (car arglist) 16) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)  
                     )             
                    ((#\O)       ; Octal   
                     (display (number->string (car arglist)  8) port) 
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f) 
                     )       
                    ((#\B)       ; Binary
                     (display (number->string (car arglist)  2) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f) 
                     )           
                    ((#\C)       ; Character 
                     (write-char (car arglist) port) 
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)  
                     )          
                    ((#\P)       ; Plural 
                     (if (= (car arglist) 1)
                       #f ; no action  
                       (write-char #\s port))
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f) 
                     )            
                    ((#\~)       ; Tilde  
                     (write-char #\~ port)   
                     (anychar-dispatch (+ pos 1) arglist #f) 
                     )            
                    ((#\%)       ; Newline   
                     (newline port) 
                     (anychar-dispatch (+ pos 1) arglist #t) 
                     )           
                    ((#\_)       ; Space 
                     (write-char #\space port)   
                     (anychar-dispatch (+ pos 1) arglist #f)
                     )             
                    ((#\&)       ; Freshline  
                     (if (not last-char-was-nl)  
                       (newline port)) 
                     (anychar-dispatch (+ pos 1) arglist #t) 
                     )         
                    ((#\T)       ; Tab -- Implementation dependent    
                     (write-char ASCII-TAB port)          
                     (anychar-dispatch (+ pos 1) arglist #f)     
                     )           
                    ((#\|)  ; Page Seperator -- Implementation dependent 
                     (write-char ascii-ff port) ;; use form-feed char   
                     (anychar-dispatch (+ pos 1) arglist #t) ; counts as newline
                     )             
                    ((#\G)       ; Pretty-print {T}     
                     (if (eq? port #f)       
                       (display (pretty-print-to-string (car arglist)) port) 
                       (pretty-print (car arglist) port))  
                     (anychar-dispatch (+ pos 1) (cdr arglist) #t) ; check this!
                     )              
                    ;; {"~?" in Common Lisp is "~K" in T}  
                    ((#\?)       ; indirection -- take next arg as format string
                     (set! last-was-newline last-char-was-nl)    
                     (anychar-dispatch 
                      (+ pos 1)        
                      (format-help (car arglist) (cdr arglist)) 
                      last-was-newline)                     
                     ; Note: format-help returns unused args 
                     )             
                    (else                
                     (error "FORMAT: unknown tilde escape"   
                            (string-ref format-strg pos))) 
                    )))        
                )) ; end tilde-dispatch   
             )                
            
            ; FORMAT-HELP MAIN      
            (anychar-dispatch 0 arglyst last-was-newline) 
            )) ; end format-help    
        
        ; FORMAT MAIN    
        (format-help <format-string> <args>)   
        (return-value)   
                                              
      ) ; end let
))) ; end format

;;---------------------------------E-O-F---------------------------------;;