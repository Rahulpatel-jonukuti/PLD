
#lang racket 

;will create ascanner


Define the scanner
(define (scanner input)
  (lexer tokens input))

; Define the parser
(define parser (lr-parser arithmetic-grammar))

; Parse an expression and display the result
(define (parse-expression input)
  (parse-string parser (scanner input)))



(define(parse a)
;here we are using the recursive _decent parsing.
(cond [(string? a)(parse!(create-scanner a))]
[(number? (peek a))(pop! a)]
[(string=?"<"(peek a)) ;here <this is from a we used 
 (let*{[-(pop! a)]
       [the-inside-expr(parse! a)]
[_(pop! a)];
 }
   (make-paren the -inside-expr))]
[(string=? "#" (peek a))
( let*{[open-hash (pop! a)]
       
   [lefty (parse! a)]
   [_(if( not (member?(peek a)OPS))
        (error 'parse" unknown op"'keep-on-going))
(pop! a)]
[op (pop! a)]
[righty (parse! a)]
[close-hash (pop! a)]
}
(make-binop lefty op righty))]
[(string=? "even"(peek a))
 (let*{[_(pop! a)]
       [_(pop! a)]
       [the-test(parse! a)]
       [_(pop! a)]}
       
       [the-even-ans (parse! a)]
       [_(pop! a)])
[the-odd-ans (parse! a)]
       [_(pop! a)]
       (make-parity the test the-even-ans the-odd-ans)
   [else( error'parse!(format "syntax error"'(peek a)))]
      
 

     
   

                          