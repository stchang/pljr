#lang slideshow

(require scheme/gui) ; for font% class
(require slideshow/code)


(define MAIN-FONT-FACE "CMU Sans Serif")
(define TT-FONT-FACE "CMU Typewriter Text")

(define (make-main-font size)
  (make-object font% size MAIN-FONT-FACE 'swiss))
(define (make-main-font-italic size)
  (make-object font% size MAIN-FONT-FACE 'swiss 'italic))
(define (make-main-font-bold size)
  (make-object font% size MAIN-FONT-FACE 'swiss 'normal 'bold))

(define (make-tt-font size)
  (make-object font% size TT-FONT-FACE 'modern 'normal 'light))
(define (make-tt-font-bold size)
  (make-object font% size TT-FONT-FACE 'modern 'normal 'normal))

(define MAIN-FONT-SIZE 24)
(define MAIN-FONT-SIZE-SMALL 20)
(define MAIN-FONT-SIZE-SMALLER 16)

(define TT-FONT-SIZE 20)
(define TT-FONT-SIZE-SMALL 16)

(current-main-font (make-main-font MAIN-FONT-SIZE))



;;-----------------------------------------------------------------------------
;; override slideshow text fns to accept additional font-size param
;; ----------------------------------------------------------------------------
(define (t txt [size MAIN-FONT-SIZE] [angle 0])
  (text txt (make-main-font size) size angle))
(define (t-small txt [angle 0])
  (t txt MAIN-FONT-SIZE-SMALL angle))
(define (it txt [size MAIN-FONT-SIZE])
  (text txt (make-main-font-italic size)))
(define (it-small txt)
  (it txt MAIN-FONT-SIZE-SMALL))
(define (bt txt [size TT-FONT-SIZE])
  (text txt (make-main-font-bold size)))
(define (bt-small txt)
  (bt txt MAIN-FONT-SIZE-SMALL))
(define (tt txt [size TT-FONT-SIZE])
  (text txt (make-tt-font size)))
(define (tt-small txt)
  (tt txt MAIN-FONT-SIZE-SMALL))
(define (btt txt [size TT-FONT-SIZE])
  (text txt (make-tt-font-bold size)))

(define-syntax (my-para stx)
  (syntax-case stx ()
    [(_ . exprs)
     #`(parameterize 
           ([current-main-font (make-main-font MAIN-FONT-SIZE)])
         (para . exprs))]))
(define-syntax (my-para-small stx)
  (syntax-case stx ()
    [(_ . exprs)
     #`(parameterize 
         ([current-main-font (make-main-font MAIN-FONT-SIZE-SMALL)])
       (para . exprs))]))

;; Pict -> Pict
;; Creates a new instance of a pict
;; For when you need a unique instance of a pict like when using pin-arrow-line
(define (pict-copy p) (cc-superimpose (blank) p))


;; underlines a pict of text
(define (underline text-pict)
  (refocus 
   (vc-append text-pict
              (hline (pict-width text-pict) 5))
   text-pict))





;; apply-f-to-index : (X -> X) Number [Listof X] -> [Listof X]
;; Applies given f to element at specified index of given list.
;; Applies f-default to all other elements. Returns new list.
(define (apply-f-to-index f f-default i lst)
  (define (helper lst counter)
    (cond
      [(empty? lst) empty]
      [else
       (if (= counter i)
           (cons (f (first lst)) (helper (rest lst) (add1 counter)))
           (cons (f-default (first lst)) (helper (rest lst) (add1 counter))))]))
  (helper lst 0))
  

;; --------------------------------------
;; connect-with-arrow- fns
;; connect 2 picts with an arrow
;; Pict Pict Number Number Number -> Pict
;; --------------------------------------
(define (connect-with-arrow-vc pict1 pict2 
                               arrow-line-length 
                               arrow-line-width 
                               arrow-head-size)
  (let
      ([group (vc-append pict1 (blank 0 arrow-line-length) pict2)])
    (pin-arrow-line arrow-head-size group pict1 cb-find pict2 ct-find 
                    #:line-width arrow-line-width)))

(define (connect-with-arrow-vc-rev pict1 pict2 
                                   arrow-line-length 
                                   arrow-line-width 
                                   arrow-head-size)
  (let
      ([group (vc-append pict1 (blank 0 arrow-line-length) pict2)])
    (pin-arrow-line arrow-head-size group pict2 ct-find pict1 cb-find 
                    #:line-width arrow-line-width)))

(define (connect-with-arrow-hc pict1 pict2 
                               arrow-line-length 
                               arrow-line-width 
                               arrow-head-size)
  (let
      ([group (hc-append pict1 (blank arrow-line-length 0) pict2)])
    (pin-arrow-line arrow-head-size group pict1 rc-find pict2 lc-find 
                    #:line-width arrow-line-width)))




;; ----------------------------------------------------------------------------
;; Xjoin-with- fns
;; Consumes non-empty list of picts and joins them with specified separator
;; to produce one combined pict
;; [listof Pict] -> Pict
;; ----------------------------------------------------------------------------
(define (hjoin-with pictlst sep)
  (foldl (λ (x acc) (ht-append acc sep x)) (first pictlst) (rest pictlst)))
(define (vjoin-with pictlst sep)
  (foldl (λ (x acc) (vc-append acc sep x)) (first pictlst) (rest pictlst)))

(define (hjoin-with-arrows pictlst)
  (hjoin-with pictlst (cc-superimpose (blank 20 20) (arrow 10 0))))

(define (vjoin-with-arrows pictlst)
  (vjoin-with pictlst (cc-superimpose (blank 0 20) (arrow 10 (* 1.5 pi)))))
  


;; ----------------------------------------------------------------------------
;; make-square-arrow- : pict pict pict Number (Number = 1 Number = 10)
;; creates square arrows connecting two picts in a group of picts
;; ----------------------------------------------------------------------------
(define (make-square-arrow-top group from-pict to-pict arrow-length 
                               (arrow-width 1) (arrow-size 10))
  (let-values
      ([(x y) (lt-find group group)]
       [(x1 y1) (ct-find group from-pict)]
       [(x2 y2) (ct-find group to-pict)]
       [(no-arrow-line) 
        (connect-with-arrow-vc (blank) (blank) 
                               arrow-length arrow-width 0)]
       [(arrow-line) 
        (connect-with-arrow-vc (blank) (blank) 
                               arrow-length arrow-width arrow-size)])
    (pin-arrow-line 
     0 (hb-append (blank (- x1 x) 0) no-arrow-line 
                  (blank (- x2 x1 x) 0) arrow-line)
     no-arrow-line lt-find arrow-line rt-find)))

(define (make-square-arrow-bot group from-pict to-pict arrow-length 
                               (arrow-width 1) (arrow-size 10))
  (let-values
      ([(x y) (lb-find group group)]
       [(x1 y1) (cb-find group from-pict)]
       [(x2 y2) (cb-find group to-pict)]
       [(no-arrow-line) 
        (connect-with-arrow-vc (blank) (blank) 
                               arrow-length arrow-width 0)]
       [(arrow-line) 
        (connect-with-arrow-vc-rev (blank) (blank) 
                                   arrow-length arrow-width arrow-size)])
    (pin-arrow-line 
     0 (ht-append (blank (- x1 x) 0) no-arrow-line 
                  (blank (- x2 x1 x) 0) arrow-line)
     no-arrow-line lb-find arrow-line rb-find)))

(define (strikethrough p)
  (cc-superimpose p (hline (pict-width p) (pict-height p))))



(define (make-title title-txt) (colorize (t title-txt) "blue"))



;; citation : String -> Pict
;; Adds brackets to given string and converts to pict of font-size 24,
;; using gray color
(define (citation ref [font-size MAIN-FONT-SIZE-SMALL])
  (colorize 
   (t (string-append "[" ref "]") font-size)
   (scale-color 1.3 "black")))

(define Kohlbecker1986 
  "Kohlbecker, Friedman, Felleisen, Duba - \"Hygienic Macro Expansion\", 1986 LFP")
(define Kohlbecker1987 
  "Kohlbecker, Wand - \"Macro-by-Example\", 1987 POPL")
(define Bawden1988 
  "Bawden, Rees - \"Syntactic Closures\", 1988 LFP")
(define Clinger1991
  "Clinger, Rees - \"Macros That Work\", 1991 POPL")
(define Dybvig1992a
  "Dybvig, Hieb, Bruggeman - \"Syntactic Abstraction in Scheme\", 1992 LSC")
(define Dybvig1992b
  "Dybvig - \"Writing Hygienic Macros in Scheme with Syntax-Case\", 1992 IU TechReport")

;; ----------------------------------------------------------------------------
;; Slides start
;; ----------------------------------------------------------------------------
(slide)
(define (make-title-slide-content 
         spacer-txt1 super-txt1 spacer-txt2 super-txt2 title-txt)
  (list 
   (vl-append
    (hb-append 
     (colorize (t-small spacer-txt1) "white")
     (colorize (t-small super-txt1 (/ pi 32)) "blue")
     (colorize (t-small spacer-txt2) "white")
     (colorize (t-small super-txt2 (/ pi 32)) "blue"))
    (colorize (t title-txt) "blue"))
   (blank)
   (t-small "Stephen Chang")
   (t-small "PL, Jr. Seminar")
   (t-small "11/15/2010")))

; Title
(slide
 (comment "")
 #:name "Title"
 'alts
 (list
  (make-title-slide-content
   " " " " " " " "
   "The History of Macros: A Guided Tour")
  (make-title-slide-content
   "The History of "
   "Scheme" 
   " " " "
   "The History of ^ Macros: A Guided Tour")
  (make-title-slide-content
   "The History of "
   "Scheme"
   "Mac"
   "(mostly)"
   "The History of ^ Macros ^: A Guided Tour")))


; Abstraction Mechanisms in Programming Languages
(slide
 (comment "Programming languages are all about abstraction. Instead of having to program in 1s and 0s, using a programming language makes programming much easier because a lot of the unncessary details are abstracted away. In addition, programming languages often provide ways for the programmer to create user-defined abstractions, for commonly used programming patterns. What are some of these abstraction mechanisms?"
          "Functions. Structs. But some usage patterns can't be expressed with procedural or data abstractions. So some languages give programmers an additional abstraction mechanism, macros. Macros are used to create syntactic abstractions.")
 #:name "Abstraction Mechanisms in Programming Languages"
 #:title "Abstraction Mechanisms in Programming Languages"
 'next
 (item "Functions (Procedural Abstraction)")
 'next
 (item "Structs, Records, Objects, etc. (Data Abstraction)")
 'next
 (item "Macros (Syntactic Abstraction)"))


;When To Use Macros
(slide
 (comment "A common question asked about macros is: can't I just use functions to do anything macros can do?")
 #:name "When To Use Macros"
 (t "Can't I just use functions to do anything macros can do?")
 'next
 (hc-append (bitmap "felleisen.jpg") (t " --- NO!"))
 'next
 (para #:align 'left "When To Use a Macro Instead of a Function:")
 'next
 (item "To introduce new binding constructs")
 'next
 (item "When the order of evaluation must be changed")
 'next
 (item "To introduce new syntax / define new DSLs")
 )

; make-macro-examples-slide
(define (make-macro-examples-slide
         comments title-txt macro-code expansion-code footnote)
  (let ([new-title-txt
         (string-append "Macro Examples: " title-txt)])
    (slide
     (comment comments)
     #:name new-title-txt
     #:title new-title-txt
     'next
     (blank)
     (blank)
     (blank)
     (blank)
     macro-code
     'next
     (tt "≡")
     expansion-code
     (blank)
     (blank)
     (blank)
     footnote)))


; Macro Examples: New Syntax
(make-macro-examples-slide
 ""
 "New Syntax"
 (vl-append
  (tt "(cond")
  (hb-append
   (tt "  ([")
   (colorize (tt "guard ") "red")
   (colorize (tt "expr") "blue")
   (tt "]"))
  (tt "       ...")
  (hb-append
   (tt "   [else ")
   (colorize (tt "else-expr") "blue")
   (tt "])")))
 (vl-append
  (hb-append 
   (tt "(if ")
   (colorize (tt "guard") "red"))
  (colorize (tt "    expr") "blue")
  (tt "    (if")
  (tt "")
  (tt "      ...")
  (tt "")
  (hb-append
   (colorize (tt "        else-expr") "blue")
   (tt "))")))
 (hb-append (t-small "(") (tt "≡") (t-small " means \"macro definition\")")))

; Macro Examples: New Binding Construct
(make-macro-examples-slide
 ""
 "New Binding Construct"
 (hb-append
  (tt "(let ([")
  (colorize (tt "x ") "red")
  (colorize (tt "val") "green")
  (tt "] ...) ")
  (colorize (tt "body") "blue")
  (tt ")"))
 (hb-append
  (tt "((λ (")
  (colorize (tt "x ...") "red")
  (tt ") ")
  (colorize (tt "body") "blue")
  (tt ") ")
  (colorize (tt "val ...") "green")
  (tt ")"))
 (t "")) ; no footnote

; Macro Examples: Change Evaluation Order
(make-macro-examples-slide
 ""
 "Change Evaluation Order"
 (hb-append (tt "(calculate-run-time ")
            (colorize (tt "expr") "blue")
            (tt ")"))
 (vl-append
  (tt "(let ([start-time (current-time)])")
  (tt "  (begin")
  (colorize (tt "    expr") "blue")
  (tt "    (- (current-time) start-time)))"))
 (t "")) ; no footnote


; When Not To Use Macros
(slide
 (comment "")
 #:name "When Not to Use Macros"
 #:title "When Not to Use Macros"
 'next
 (t "Do not use macros for efficiency /  inlining!!!")
 (t-small "(like C programmers used to do)"))



; Naive Macro Expansion
(slide
 (comment "")
 #:name "Naive Macro Expansion"
 #:title "Naive Macro Expansion"
 (vl-append
  (t "1) find all macro calls in a program and expand")
  (t "2) repeat with expanded program until there are no more macro calls"))
 'next
 (t "Used in languages like C and Lisp"))

; Naive Macro Expansion Problem: Hygiene
(slide
 (comment "")
 #:name "Naive Macro Expansion Problem: Hygiene"
 #:title "Naive Macro Expansion Problem: Hygiene"
 (t "Macro Definition:")  
 (vl-append
 (tt "(or e1 e2)")
 (hb-append (tt "     ") (tt "≡"))
 (tt "(let ([v e1])")
 (tt "  (if v v e2))"))
 'next
 (blank)
 (t "Macro Use:")
 (vl-append
  (tt "(let ([v true]))")
  (tt "  (or false v)"))
 'next
 (t-small "macro expands to")
 (vl-append
  (tt "(let ([v true])")
  (tt "  (let ([v false])")
  (tt "    (if v v v))"))
 'next
 'alts
 (list
  (list (frame (vl-append
                (t-small "Macro Hygiene: Variable bindings introduced by a macro should")
                (t-small "not capture variables used at the macro call site."))))
  (list (frame (vl-append
                (t-small "Macro Hygiene: Variable bindings introduced by a macro should")
                (strikethrough (t-small "not capture variables used at the macro call site."))
                (t-small "only bind variables introduced by that macro."))))))
 

; Bad Ways to Address Hygiene
(slide
 (comment "Painful to program this way. Errors are hard to detect because macro will \"work\" most of the time. Macros generating macros may will have variable name conflicts.")
 #:name "Bad Ways To Address Hygiene"
 #:title "Bad Ways To Address Hygiene"
 (item "Use obscure variable names in the macro")
 'next
 (subitem (tt "_____macro_variable_____"))
 'next
 (subitem (tt "____dont_use_me_as_a_variable___"))
 'next
 (item "The programmer should just be careful"))

;; ----------------------------------------------------------------------------
;;Kohlbecker, Friedman, Felleisen, Duba - Hygienic Macro Expansion

(slide
 (comment "")
 #:name Kohlbecker1986
 (citation Kohlbecker1986)
 (my-para-small "\"... the task of safely renaming macro-generated identifiers is mechanical. It is essentially an α-conversion which is knowledgeable about the origin of identifiers. For these reasons we propose a change to the naive macro expansion algorithm which automatically maintains hygienic conditions during expansion time.\""))

(slide
 (comment "Can't apply α-conversions at every step because 1) you dont know that user context, and 2) there may be future expansions that still capture your generated variable")
 #:name Kohlbecker1986
 (citation Kohlbecker1986)
 (my-para-small "\"From the λ-calculus, one knows that if the hygiene condition does not hold, it can be established by an appropriate number of α-conversions. That is also the basis of our solution. Ideally, α-conversions should be applied with every transformation step, but that is impossible. One cannot know in advance which macro-generated identifier will end up in a binding position Hence it is a quite natural requirement that one retains the information about the origin of an identifier. To this end, we combine the expansion algorithm with a tracking mechanism.\""))

(slide
 (comment "")
 #:name Kohlbecker1986
 (citation Kohlbecker1986)
 (my-para-small "\"Tracking is accomplished with a time-stamping scheme. Time-stamps, sometimes called clock values, are simply non-negative integers. The domain of time-stamped variables is isomorphic to the product of identifiers and non-negative integers.\""))

(slide
 (comment "")
 #:title "Algorithm"
 #:name Kohlbecker1986
 (citation Kohlbecker1986)
 (my-para-small "1) stamp all variables in initial program with time-stamp 0")
 (my-para-small "2) do macro expansion")
 (my-para-small "3) increment time-stamp counter and stamp new variables")
 (my-para-small "4) repeat steps 2 and 3 until there are no more macro calls")
 (my-para-small "5) rename time-stamped variables that only differ in their timestamps")
 (my-para-small "6) remove all time-stamps"))

(slide
 (comment "")
 #:title "Algorithm Example"
 #:name Kohlbecker1986
 (citation Kohlbecker1986)
 (vl-append
  (tt "(define v true)")
  (tt "(or false v)"))
 'next
 (t-small "macro expands to")
 (vl-append
  (tt "(define v:0 true)")
  (tt "(let ([v:1 false]) (if v:1 v:1 v:0))")))

;; ----------------------------------------------------------------------------
;; Kohlbecker, Wand - Macro-by-Example: Deriving Syntactic Transformations from their Specifications, 1987 POPL

(slide
 (comment "Used to have to manually traverse syntax tree with cars and cdrs, or use quasiquoting")
 #:name Kohlbecker1987
 (citation Kohlbecker1987)
 (my-para-small "\"Even in languages such as Lisp that allow syntactic abstractions, the process of defining them is notoriously difficult and error-prone.\""))

(slide
 (comment "Used to have to manually traverse syntax tree with cars and cdrs, or use quasiquoting. syntax-rules first appeared in the Scheme Report as an appendix to R4RS in 1991, and became officially a part of the Scheme specification in R5RSin 1998")
 #:title "Contributions"
 #:name Kohlbecker1987
 (citation Kohlbecker1987)
 (my-para-small "predecessor to" (tt-small "syntax-rules") " - allows definition of macros that resembles specification language")
 (item "pattern matching")
 (item "ellipses matches repetitive elements")
 (item "multiple cases")
 (item "fenders on each case")
 (item "hygiene (using Kohlbecker, et al. 1986)"))

; syntax-rules example - named let??? (named let needs capturing???)
(slide
 (comment "")
 #:name "syntax-rules example"
 #:title (hb-append (tt "syntax-rules") (t " example")))



;; ----------------------------------------------------------------------------
;; Bawden, Rees - Syntactic Closures
(slide
 (comment "")
 #:name Bawden1988
 (citation Bawden1988)
 (my-para-small "\"\"Hygienic macro expansion\" (of Kohlbecker, et al.) is the only other complete solution to the macro scoping problems of which we are aware. Hygienic expansion works by \"painting\" the entire input expression with some distinctive color before passing it in to the expander. Then the returned replacement expression is examined to find those parts that originated from the input expression; these can be identified by their color. The names in the unpainted text are protected from capture by the painted text, and vice versa.\""))

(slide
 (comment "")
 #:name Bawden1988
 (citation Bawden1988)
 (my-para-small "\"The painting is done without any understanding of the syntax of the input expression. Paint is applied to expressions, quoted constants, cond-clauses, and the bound variable lists from lambda-expressions. This strikes us as being very undisciplined. We prefer a scheme that is everywhere sensitive to the underlying syntactic and semantic structure of the language. In addition, it is difficult to comprehend how hygienic expansion operates and why it is correct.\"")
 'next
 (blank)
 (blank)
 (my-para-small "\"We feel that syntactic closures solve scoping problems in a natural, straightforward way.\""))


(slide
 (comment "Instead of just operating on syntax, macros now work on syntax + lexical scoping information.")
 #:name Bawden1988
 (citation Bawden1988)
 (my-para-small "\"In the same way that closures of lambda-expressions solve scoping problems at run time, we propose to introduce syntactic closures as a way to solve scoping problems at macro-expansion time.\""))









;; ----------------------------------------------------------------------------
;; Clinger, Rees - Macros That Work

(slide
 (comment "")
 #:name Clinger1991
 (citation Clinger1991)
 (my-para-small "\"The problem with syntactic closures is that they are inherently low-level and therefore difficult to use correctly, especially when syntactic keywords are not reserved. It is impossible to construct a pattern-based, automatically hygienic macro system on top of syntactic closures because the pattern interpreter must be able to determine the syntactic role of an identifier (in order to close it in the correct syntactic environment) before macro expansion has made that role apparent.\""))

(let ([val-colored (colorize (tt "val") "green")]
      [x-colored (colorize (tt "x") "red")]
      [body-colored (colorize (tt "body") "blue")])
  (slide
   (comment "")
   #:name Clinger1991
   (vc-append
    (hb-append
     (tt "(let ([")
     x-colored (tt " ")
     val-colored
     (tt "] ...) ")
     body-colored
     (tt ")"))
    (tt "≡")
    (hb-append
     (tt "((λ (")
     (colorize (tt "x ...") "red")
     (tt ") ")
     body-colored
     (tt ") ")
     (colorize (tt "val ...") "green")
     (tt ")")))
   (citation Clinger1991)
   (my-para-small "\"Consider the" (tt-small "let") " macro. When this macro is used the" val-colored "expressions must be closed in the syntactic environment of the use, but the" body-colored "cannot simply be closed in the syntactic environment of the use because its references to" x-colored "must be left free. The pattern interpreter cannot make this distinction unaided until the lambda expression is expanded, and even then it must somehow remember that the bound variable of the lambda expression came from the original source code and must therefore be permitted to capture the references that occur in" body-colored ".\"")))



(slide
 (comment "")
 #:name Clinger1991
 (citation Clinger1991)
 (my-para-small "\"This paper unifies and extends the competing paradigms of hygienic macro expansion and syntactic closures to obtain an algorithm that combines the benefits of both.\""))
 
(slide
 (comment "")
 #:name Clinger1991
 (citation Clinger1991)
 (my-para-small "\"Unlike previous algorithms, our algorithm runs in linear instead of quadratic time.\"")
 'next
 (my-para-small "\"The reason that previous algorithms for hygienic macro expansion are quadratic in time is that they expand each use of a macro by performing naive expansion followed by an extra scan of the expanded code to find and paint (i.e. - rename, or time-stamp) the newly introduced identifiers. If macros expand into uses of still other macros with more or less the same actual parameters, which often happens, then large fragments of code may be scanned anew each time a macro is expanded.\"")
 'next
 (my-para-small "\"Our algorithm runs in linear time because it finds the newly introduced identifiers by scanning the rewrite rules, and paints these identifiers as they are introduced during macro expansion. The algorithm therefore scans the expanded code but once, for the purpose of completing the recursive expansion of the code tree, just as in the naive macro expansion algorithm.\""))

(slide
 (comment "")
 #:name Clinger1991
 (citation Clinger1991)
 (my-para-small "\"Consider an analogy from lambda calculus. In reducing an expression to normal form by textual substitution, it is sometimes necessary to rename variables as part of a beta reduction. It doesn't work to perform all the (naive) beta reductions first, without renaming, and then to perform all the necessary alpha conversions; by then it is too late. Nor does it work to do all the alpha conversions first, because beta reductions introduce new oppotunities for name clashes. The renamings must be" (it-small "interleaved") "with the (naive) beta reductions, which is the reason why the notion of substitution required by the non-naive beta rule is so complicated.\"")
 (my-para-small "\"The same situation holds for macro expansions. It does not work to simply expand all macro calls and then rename variables, nor can the renamings be performed before expansion. The two processes must be interleaved in an appropriate manner. A correct and efficient realization of this interleaving is our primary contribution.\""))




;; Naive Macro Expansion Problem: Referential Transparency
(slide
 (comment "")
 #:name "Naive Macro Expansion Problem: Referential Transparency"
 #:title "Naive Macro Expansion Problem: Referential Transparency"
 (t "Macro Definition:")  
 (vl-append
  (tt "(or e1 e2)")
  (hb-append (tt "     ") (tt "≡"))
  (tt "(let ([v e1])")
  (tt "  (if v v e2))"))
 'next
 (blank)
 (t "Macro Use:")
 (vl-append
  (tt "(let ([if and]))")
  (tt "  (or false true)"))
 'next
 (t-small "macro expands to")
 (vl-append
  (tt "(let ([if and])")
  (tt "  (let ([v false])")
  (tt "    (if v v true))"))
)


(slide
 (comment "emphasis mine")
 #:name Clinger1991
 (citation Clinger1991)
 (my-para-small "\"We would like for free variables that occur on the right hand side of a rewriting rule for a macro to be resolved in the lexical environment of the macro" (bt-small "definition") "instead of being resolved in the lexical environment of the" (bt-small "use") "of the macro.\"")
 'next
 (my-para-small #:fill? #f "...")
 (my-para-small "\"Our algorithm supports referentially transparent macros.\""))



(slide
 (comment "")
 #:name Clinger1991
 (citation Clinger1991)
 (my-para-small "\"A high-level macro system similar to that described here is currently implemented on top of a compatible low-level system that is not described in this paper.\""))




;; ----------------------------------------------------------------------------
;; Dybvig, Hieb, Bruggeman - Syntactic Abstraction in Scheme, 1992 LSC
;; Dybvig - Writing Hygienic Macros in Scheme with Syntax-Case, 1992 IU TechReport

(slide
 (comment "")
 #:name Dybvig1992a
 (citation Dybvig1992a)
 (citation Dybvig1992b))

(slide
 (comment "")
 #:title "Contributions"
 #:name Dybvig1992a
 (citation Dybvig1992a)
 (citation Dybvig1992b)
 (item "syntax-case")
 (subitem "combines high-level and low-level systems - can write unhygienic macros")
 (subitem "referential transparency")
 (subitem "syntax objects = syntax + lexical info + source locations"))

(slide
 (comment "")
 #:name Dybvig1992a
 (citation Dybvig1992a)
 (citation Dybvig1992b)
 (my-para-small "\"Lisp macro systems cannot track source code through the macro-expansion process. Reliable correlation of course code and macro-expanded code is necessary if the compiler, run-time system, and debugger are to communicate with the programm in terms of the original source program.\""))