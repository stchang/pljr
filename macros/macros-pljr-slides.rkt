#lang slideshow

(require scheme/gui) ; for font% class
(require slideshow/code)


(define MAIN-FONT-FACE "CMU Sans Serif")
(define TT-FONT-FACE "CMU Typewriter Text")

(define (make-main-font size)
  (make-object font% size MAIN-FONT-FACE 'swiss))
(define (make-main-font-italic size)
  (make-object font% size MAIN-FONT-FACE 'swiss 'italic))

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
(define (tt txt [size TT-FONT-SIZE])
  (text txt (make-tt-font size)))
(define (btt txt [size TT-FONT-SIZE])
  (text txt (make-tt-font-bold size)))

(define (my-para txt)
  (parameterize 
      ([current-main-font (make-main-font MAIN-FONT-SIZE)])
    (para txt)))
(define (my-para-small txt)
  (parameterize 
      ([current-main-font (make-main-font MAIN-FONT-SIZE-SMALL)])
    (para txt)))

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




;; img constants
(define LAM (tt "λ"))
(define DOT (tt "."))
(define LPAR (tt "("))
(define RPAR (tt ")"))
(define SPC (tt " "))
(define EQ (tt "="))
(define NOTIN (text "Ï" 'symbol TT-FONT-SIZE))

(define M (tt "M"))
(define M0 (tt "M0"))
(define M1 (tt "M1"))
(define M2 (tt "M2"))
(define M3 (tt "M3"))
(define M4 (tt "M4"))
(define M5 (tt "M5"))
(define lam1 (pict-copy LAM))
(define lam2 (pict-copy LAM))
(define lam3 (pict-copy LAM))



;; citation : String -> Pict
;; Adds brackets to given string and converts to pict of font-size 24,
;; using gray color
(define (citation ref)
  (colorize 
   (t (string-append "[" ref "]") MAIN-FONT-SIZE-SMALL) 
   (scale-color 1.3 "black")))

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


; some reduction arrow constants
(define REDUCE-ARR (connect-with-arrow-hc (t " ") (t " ") 40 2 10))
(define SR-ARR (vc-append -16 (tt "SR" 16) REDUCE-ARR))


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





(define (make-title title-txt) (colorize (t title-txt) "blue"))

;; ----------------------------------------------------------------------------
;; Slides start
;; ----------------------------------------------------------------------------
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
   "The History o"
   "Scheme" 
   " " " "
   "The History of ^ Macros: A Guided Tour")
  (make-title-slide-content
   "The History o"
   "Scheme"
   "Ma"
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
 #:title "When To Use Macros"
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
         (string-append "Macro Examples - " title-txt)])
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


; Macro Examples - New Syntax
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
 (t-small "(≡ means \"macro definition\")"))

; Macro Examples - New Binding Construct
(make-macro-examples-slide
 ""
 "New Binding Construct"
 (hb-append
  (tt "(let ([")
  (colorize (tt "x ") "red")
  (colorize (tt "val") "green")
  (tt "] ...) ")
  (colorize (tt "expr") "blue")
  (tt ")"))
 (hb-append
  (tt "((λ (")
  (colorize (tt "x ...") "red")
  (tt ") ")
  (colorize (tt "expr") "blue")
  (tt ") ")
  (colorize (tt "val ...") "green")
  (tt ")"))
 (t "")) ; no footnote

; Macro Examples - Change Evaluation Order
(make-macro-examples-slide
 ""
 "Change Evaluation Order"
 (hb-append (tt "(run-time ")
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



; Macro Problems - Hygiene
(slide
 (comment "")
 #:name "Macro Problems - Hygiene"
 #:title "Macro Problems - Hygiene"
 (vl-append
 (tt "(or e1 e2)")
 (tt "      ≡")
 (tt "(let ([v e1])")
 (tt "  (if v v e2))"))
 'next
 (blank)
 (blank)
 (vl-append
  (tt "(define v true)")
  (tt "(or false v)"))
 (t-small "macro expands to")
 (vl-append
  (tt "(define v true)")
  (tt "(let ([v false]) (if v v v))"))
 (frame (my-para-small "Macro Hygiene: Variable bindings introduced by a macro should not capture variables used at the macro call site")))


; Bad Ways to Address Hygiene
(slide
 (comment "Painful to program this way. Errors are hard to detect because macro will \"work\" most of the time. Macros generating macros may will have variable name conflicts.")
 #:name "Bad Ways To Address Hygiene"
 #:title "Bad Ways To Address Hygiene"
 (item "Use obscure variable names in the macro")
 (item "The programmer should just be careful"))

; Kohlbecker, Friedman, Felleisen, Duba - Hygienic Macro Expansion
(slide
 (comment "")
 #:name "Kohlbecker, Friedman, Felleisen, Duba - Hygienic Macro Expansion"
 (citation "Kohlbecker, Friedman, Felleisen, Duba - Hygienic Macro Expansion, 1986 LFP")
 (my-para-small "\"... the task of safely renaming macro-generated identifiers is mechanical. It is essentially an α-conversion which is knowledgeable about the origin of identifiers. For these reasons we propose a change to the naive macro expansion algorithm which automatically maintains hygienic conditions during expansion time.\""))

; Kohlbecker, Friedman, Felleisen, Duba - Hygienic Macro Expansion
(slide
 (comment "Can't apply α-conversions at every step because 1) you dont know that user context, and 2) there may be future expansions that still capture your generated variable")
 #:name "Kohlbecker, Friedman, Felleisen, Duba - Hygienic Macro Expansion"
 (citation "Kohlbecker, Friedman, Felleisen, Duba - Hygienic Macro Expansion, 1986 LFP")
 (my-para-small "\"From the λ-calculus, one knows that if the hygiene condition does not hold, it can be established by an appropriate number of α-conversions. That is also the basis of our solution. Ideally, α-conversions should be applied with every transformation step, but that is impossible. One cannot know in advance which macro-generated identifier will end up in a binding position Hence it is a quite natural requirement that one retains the information about the origin of an identifier. To this end, we combine the expansion algorithm with a tracking mechanism.\"")
 'next
 (my-para-small "\"Tracking is accomplished with a time-stamping scheme. Time-stamps, sometimes called clock values, are simply non-negative integers. The domain of time-stamped variables is isomorphic to the product of identifiers and non-negative integers.\""))































; Our Paper Slide
(let
    ([bullet1 (item (t "New way to resolve variable references in the stack"))]
     [bullet2 (item (t "Reorganize stack structure to allow indexing"))]
     [make-alt (λ (bullet1 bullet2) (list (vl-append 40 bullet1 bullet2)))])
  (slide
   #:name "Our Paper"
   #:title (make-title "Our Paper")
   (comment "We introduce a new lazy abstract machine that improves on Garcia's approach. Our main contribution is a novel way of resolving variable references in the stack. We'll see that finding an argument in the stack is an issue because the argument could be many stack frames away. We try to address this issue by introducing a special stack structure that enables the use of static addresses to directly index into the dynamic control stack. This is the first machine that we know of that does this.\n\n"
            "We will explain our machine in more detail after a few background slides.")
   'alts
   (list
    (make-alt bullet1 (ghost bullet2))
    (make-alt bullet1 bullet2))
   ))





;; call-by-need λ-calculus slide
(let*
    ([redex (tt "(λx.E[x]) V")]
     [redactum (hc-append (tt "(λx.E[") (btt "V") (tt "]) V"))]
     [E=-text (tt " E = ")]
     [E= (tt " E = ")]
     [E=-bold (hc-append (btt " ") E=)]
     [E1-text "[ ] | E M"]
     [E1 (tt E1-text)]
     [E1-bold (btt E1-text)]
     [E2-text "(λx.E) M"]
     [E2 (tt E2-text)]
     [E2-bold (btt E2-text)]
     [E3-text "(λx.E[x]) E"]
     [E3 (tt E3-text)]
     [E3-bold (btt E3-text)]
     [sep (tt " | ")]
     [initial-Es (hc-append E=-bold E1 sep E2 sep E3)]
     )
  (slide
   #:name "Call-by-need λ-Calculus"
   #:title "Call-by-need λ-Calculus"
   (comment
    "Our lazy abstract machine, as well as that of Garcia and his co-authors, is derived from the call by need calculus of Ariola and Felleisen. The calculus can be summarized by two key properties. The first is that arguments in an application are not evaluated until they are required by the procedure body. And the second is that each argument should only be evaluated once. This is the concept of sharing.\n\n"
    "Now let's see how the calculus formalizes these ideas. The terms of the calculus are the same as in call-by-value or call-by-name. The difference is in the evaluation order, as specified by the two key points above.\n\n"
    "Ariola and Felleisen use evaluation contexts to specify the desired evaluation order. The first two contexts are the same as in call-by-name and the second context specifies a leftmost, outermost evaluation order. The next context is where we see evaluation of the argument getting delayed. It says that when we see an application and the operator is a lambda, temporarily ignore the argument and start evaluating the body of the operator. The last evaluation context specifies when arguments get evaluated. Only when you run into a variable in the procedure body do you start reducing the argument corresponding to that variable.\n\n"
    "So far I've explained how the calculus satisfies the first property at the top of the slide. What about the second one? for the second property, we need a notion of reduction for the calculus. Ariola and Felleisen introduce the deref reduction as an alternative to β substitution because we only want to substitute for variables as they are needed.  There are two things to notice about the reduction. The first is that variables are substituted for one at a time. The second is that the evaluated argument does not get removed after substitution, because it may be needed again. These two properties of the reduction capture the sharing in call by need because the next time a variable looks for this argument, it will already be evaluated."
    )
   (vc-append 1 
              (citation "Ariola et al. 1995")
              (citation "Ariola and Felleisen 1997"))
   'next
   (item "Delay evaluation of argument until needed")
   'next
   (item "Evaluate each argument only once")
  'next
   (tt "M = x | M M | λx.M")
   'next
   'alts
   (list
    (list initial-Es)
    (list (hc-append E= E1-bold sep E2 sep E3))
    (list (hc-append E= E1 sep E2-bold sep E3))
    (list (hc-append E= E1 sep E2 sep E3-bold))
    (list initial-Es)
    )
   (hc-append (it "deref ") 
              (t "(β alternative):" MAIN-FONT-SIZE-SMALL))
   (hc-append (hc-append redex REDUCE-ARR redactum))
   'next
   (item "One-at-a-time substitution (only when needed)")
   'next
   (item "Argument not removed (may need it again)")
   ))




;; Initial Abstract Machine Slide
(slide
 #:name "An Initial Abstract Machine"
 #:title (make-title "An Initial Abstract Machine")
 (comment "So now we have a calculus, which is a model for a call by need language. However, if we want to implement a real programming language, we need to specify the semantics of our language at a lower level. This is where abstract machines are useful.\n\n"
          "So how do we go from the call by need calculus to a lazy abstract machine? Well, Ariola and Felleisen show that the call by need calculus has a standard reduction property, which says that if there is some way of reducing a term to an answer, then there is a canonical way of reducing the term to the answer. And this canonical reduction strategy IS an abstract machine where the transitions of the machine are just reductions in the calculus.\n\n"
          "However, with this machine, the entire program must be re-partitioned into a redex and an evaluation context after every reduction, which is undesirable.")
 'next
   (t "Standard Reduction = abstract machine")
   (hc-append (tt "E[M]") SR-ARR (tt "E[N]"))
   (hc-append (t "if ") (tt "M") REDUCE-ARR (tt "N"))
   'next
   (item "Re-partition into E and M after every reduction")
 )






; CK Machine Slide
(slide
 #:name "CK Machine"
 #:title (make-title "CK Machine")
 (comment 
  "Instead, we use the strategy from Felleisen's CK machine, which is an abstract machine for the call-by-value lambda calculus. The CK machine splits its machine state into two separate registers. There is one register (C) for the current subterm being evaluated and one (K) for the context. The CK machine uses continuations to represent the context. I'll talk more about the continuation representation in the next slide. The K register can also be thought of as the control stack of the machine, with each continuation being a frame in the stack. In the CK machine, after a reduction, the machine resumes evaluation from the last partitioning instead of having to re-partition the entire program.\n\n"
          "Garcia and his coauthors also use this strategy of having separate registers to come up with their lazy abstract machine, which is essentially a lazy CK machine.")
 (citation "Felleisen 1986")
 (t "(For by-value λ calculus)" MAIN-FONT-SIZE-SMALL)
 (item "Separate program into two registers:")
 (subitem (btt "C") (t " = Current subterm being evaluated"))
 (subitem (btt "K") 
          (t " = Continuation (equiv. to eval. context)"))
 'next
 (frame (inset (t "Don't need to re-partition program after every reduction") 24))
 'next
 (hc-append (citation "Garcia et al. 2009") (t ": lazy CK machine"))
 )






;; E vs K slide
(let*
    ([make-K-pict (λ (K-pict side-conditions) 
                    (vc-append 
                     (vc-append 0 K-pict side-conditions)
                     (blank MAIN-FONT-SIZE)))]
     [EK-text "E ~ K"]
     [EK2-text "K' ~ E', K ~ E"]
     [EK (tt EK-text)]
     [EK2 (tt EK2-text)]
     [mt-text "[ ] ~ mt"]
     [mt (make-K-pict (tt mt-text) (blank))]
     [mt-bold (make-K-pict (btt mt-text) (blank))]
     [arg-text "E[[ ] M] ~ (arg M K)"]
     [arg (make-K-pict (tt arg-text) EK)]
     [arg-bold (make-K-pict (btt arg-text) EK)]
     [bind-text "E[(λx.[ ]) M] ~ (bind x M K)"]
     [bind (make-K-pict (tt bind-text) EK)]
     [bind-bold (make-K-pict (btt bind-text) EK)]
     [op-text "E[(λx.E'[x]) [ ]] ~ (op x K' K)"]
     [op (make-K-pict (tt op-text) EK2)]
     [op-bold (make-K-pict (btt op-text) EK2)]
     )
    (slide
     #:name "Evaluation Contexts (E) vs Continuations (K)"
     #:title (make-title "Evaluation Contexts (E) vs Continuations (K)")
 (comment 
  "In this slide, I'm going to briefly introduce the continuation representation of contexts and compare them to their equivalent evaluation contexts. This representation is more or less the same in our machine and in Garcia's machine.\n\n"
  "In this slide the evaluation contexts are on the left and the continuations are on the right. Remember, the continuations are the K in the CK machine. The mt continuation represents the empty context. The arg continuation holds the argument in an application term. (We can see here that continuations are just ``inside-out'' versions of evaluation contexts because on the left we have the E on the outside and on the right we have the K on the inside. And this is true for all the following continuations as well. So with continuations, the innermost context is the most easily accessible.) The bind continuation represents the binding of a variable in an application. Finally, the op continuation represents the site of a variable when the argument is being evaluated. One thing to note is that the information in a bind frame overlaps with information in an arg frame. This point be relevant in a future slide.\n\n"
  "The site of a variable reference is saved by copying the continuations under the lambda, which is this K' here. When the machine is done evaluating the argument it can jump right back to the location of the variable reference by restoring the K' continuation.")
 'alts
 (list
  (list mt arg bind op)
  (list mt-bold arg bind op)
  (list mt arg-bold bind op)
  (list mt arg bind-bold op)
  (list mt arg bind op-bold)
  )
 ))











;; Garcia Machine Example Slide
(let*
    ([Ks (list "(arg M)" "(bind z M0)" "(arg M1)" "(arg M2)" "(bind y M3)" "(arg M4)" "(bind x M5)" "mt")]
     [var1 (tt "x")]
     [var2 (tt "y")]
     [var3 (tt "z")]
     [term-pict (tt "(λx.(λy.(λz.(y M)) M0 M1 M2) M3 M4) M5")]
     [term-pict-chars (hc-append LPAR lam1 var1 DOT LPAR lam2 var2 DOT LPAR lam3 var3 DOT LPAR var2 
                                 SPC M RPAR RPAR SPC M0 SPC M1 SPC M2 RPAR SPC M3 SPC M4 RPAR SPC M5)]
     [term-pict-arrows (lb-superimpose
                        (make-square-arrow-top term-pict-chars lam1 M5 60)
                        (make-square-arrow-top term-pict-chars lam2 M3 40)
                        (make-square-arrow-top term-pict-chars lam3 M0 20))]
     [slide-font-size 17]
     [K-picts (hjoin-with-arrows
               (map (λ (t) (tt t slide-font-size)) Ks))]
     [eq-pict (tt " = ")]
     [make-step-pict-with-Kpicts
      (λ (term-pre term-underline term-post current-K-picts)
        (list (vc-append
               (vl-append
                term-pict-arrows
                (hc-append (tt term-pre)
                           (underline (tt term-underline))
                           (tt term-post)))
               (blank 0 (current-font-size))
               (vl-append
                (hc-append (btt "C") eq-pict 
                           (tt term-underline slide-font-size))
                (hc-append (btt "K") eq-pict 
                           (lc-superimpose
                            (hjoin-with-arrows current-K-picts)
                            (ghost K-picts)))))))]
     [render-Ks (λ (Ks) (map (λ (t) (tt t slide-font-size)) Ks))]
     [make-step-pict
      (λ (term-pre term-underline term-post current-Ks)
        (make-step-pict-with-Kpicts term-pre term-underline term-post
                                    (render-Ks current-Ks)))]
     )
  (slide
   #:name "Lazy CK: Example (Garcia)"
   #:title (make-title "Example (Garcia Machine)")
   (comment 
    "Here is an example program. For clarity I've drawn arrows from each lambda to its argument. This program may look complicated so let me try to explain it a little bit. The outer level term is just an application, where the argument is M5 and the operator is already a lambda. In the body of this lambda are two more applications. And note that I'm assuming that application associates to the left. The leftmost term in the second level applications is again a lambda. The body of this lambda is three more applications. Finally, the leftmost term in these applications is another lambda, where the body is one final application that contains a variable. I'm now going to show part of the evaluation of this program, first with Garcia's machine, and then with our machine, and in between I'm going to show the differences between the two machines.\n\n"
    "Here is the initial machine configuration, so the entire program is in the C register. In this slide, the contents of the C register will always be underlined in the term so you can see it better. The outermost term is an application so the machine wants to evaluate the operator, which means we need to save the argument. So the machine pushes the argument onto the stack in an arg frame. In the continuation register, I've replaced the nested K inside each continuation with an arrow. Now we see the operator in the application is already a lambda so we need to go under the lambda. When we go under the lambda, we need to remember both the variable and the argument so we need to create a bind frame. I previously mentioned that the information in a bind continatuation overlaps with an arg. So instead of pushing a new bind frame, the machine replaces the arg frame in the stack with a bind. Now under the lambda, we see some more applications so we push some more arguments onto the stack. For the operator, we see another lambda so we create another bind frame, replacing the last arg frame. And so on until we get to just the variable y in the C register.\n\n"
    "At this point, we know that the argument corresponding to this variable is needed, so the machine searches the control stack until finds that argument. After that, the machine will start evaluating the argument. Thus far I have been describing the lazy abstract machine of Garcia and his co-authors. One of the things we noticed is that searching for an argument is potentially an expensive operation because you may need to search the entire control stack. This leads us to the main contribution of our paper.")
   'alts
   (append
    (list
     (list (vl-append (ghost term-pict-arrows) term-pict))
     (list (vl-append term-pict-arrows term-pict))
     (make-step-pict 
      "" "(λx.(λy.(λz.(y M)) M0 M1 M2) M3 M4) M5" ""
      (list "mt"))
     (make-step-pict
      "" "(λx.(λy.(λz.(y M)) M0 M1 M2) M3 M4)" " M5"
      (list "(arg M5)" "mt"))
     (make-step-pict
      "(λx." "(λy.(λz.(y M)) M0 M1 M2) M3 M4" ") M5"
      (list "(bind x M5)" "mt"))
     (make-step-pict
      "(λx." "(λy.(λz.(y M)) M0 M1 M2) M3" " M4) M5"
      (list "(arg M4)" "(bind x M5)" "mt"))
     (make-step-pict
      "(λx." "(λy.(λz.(y M)) M0 M1 M2)" " M3 M4) M5"
      (list "(arg M3)" "(arg M4)" "(bind x M5)" "mt"))
     (make-step-pict
      "(λx.(λy." "(λz.(y M)) M0 M1 M2" ") M3 M4) M5"
      (list "(bind y M3)" "(arg M4)" "(bind x M5)" "mt"))
     (make-step-pict
      "(λx.(λy." "(λz.(y M)) M0 M1" " M2) M3 M4) M5"
      (list "(arg M2)" "(bind y M3)" "(arg M4)" "(bind x M5)" "mt"))
     (make-step-pict
      "(λx.(λy." "(λz.(y M)) M0" " M1 M2) M3 M4) M5"
      (list "(arg M1)" "(arg M2)" "(bind y M3)" "(arg M4)" "(bind x M5)" "mt"))
     (make-step-pict
      "(λx.(λy." "(λz.(y M))" " M0 M1 M2) M3 M4) M5"
      (list "(arg M0)" "(arg M1)" "(arg M2)" 
            "(bind y M3)" "(arg M4)" "(bind x M5)" "mt"))
     (make-step-pict
      "(λx.(λy.(λz." "(y M)" ") M0 M1 M2) M3 M4) M5"
      (list "(bind z M0)" "(arg M1)" "(arg M2)" 
            "(bind y M3)" "(arg M4)" "(bind x M5)" "mt"))
     (make-step-pict
      "(λx.(λy.(λz.(" "y" " M)) M0 M1 M2) M3 M4) M5"
      Ks)
     )
    (map
     (λ (lst)
       (make-step-pict-with-Kpicts
        "(λx.(λy.(λz.(" "y" " M)) M0 M1 M2) M3 M4) M5"
        lst))
     (map 
      (λ (i)
        (apply-f-to-index
         (λ (t) (btt t slide-font-size))
         (λ (t) (tt t slide-font-size))
         i Ks))
     (append (build-list (- (length Ks) 3) (λ (x) x)) '(4 4 4))))
    )
   'next
   (item #:fill? false " Linear search to find argument")
   ))






;; CK+: Stack Slide
(let*
    ([slide-font-size 17]
     [Ks (list "(arg M)" "(bind z M0)" "(arg M1)" "(arg M2)" "(bind y M3)" "(arg M4)" "(bind x M5)" "mt")]
     [K-picts (map (λ (t) (tt t slide-font-size)) Ks)]
     [K-picts-bold (list (tt "(arg M)" slide-font-size) 
                         (btt "(bind z M0)" slide-font-size)
                         (tt "(arg M1)" slide-font-size)
                         (tt "(arg M2)" slide-font-size)
                         (btt "(bind y M3)" slide-font-size)
                         (tt "(arg M4)" slide-font-size)
                         (btt "(bind x M5)" slide-font-size)
                         (tt "mt" slide-font-size))]
     [Ks-pict (hjoin-with-arrows K-picts)]
     [Ks-pict-bold (hjoin-with-arrows K-picts-bold)]
     [K+s (list (list "(arg M)" "mt")
                (list "(bind M0)" "(arg M1)" "(arg M2)" "mt")
                (list "(bind M3)" "(arg M4)" "mt")
                (list "(bind M5)" "mt"))]
     [K+-picts-bold (list (list (tt "(arg M)" slide-font-size)
                                (tt "mt" slide-font-size))
                          (list (btt "(bind M0)" slide-font-size) 
                                (tt "(arg M1)" slide-font-size)
                                (tt "(arg M2)" slide-font-size)
                                (tt "mt" slide-font-size))
                          (list (btt "(bind M3)" slide-font-size) 
                                (tt "(arg M4)" slide-font-size) 
                                (tt "mt" slide-font-size))
                          (list (btt "(bind M5)" slide-font-size)
                                (tt "mt" slide-font-size)))]
     [K+-picts (map (λ (lst) 
                      (vjoin-with-arrows
                       (map (λ (t) (tt t slide-font-size)) lst))) 
                    K+s)]
     [K+s-pict (hjoin-with-arrows K+-picts)]
     [K+s-pict-bold (hjoin-with-arrows 
                     (map (λ (lst) (vjoin-with-arrows lst)) K+-picts-bold))]
     [connecting-down-arrow (colorize (cc-superimpose (blank 0 100)
                                                      (scale (arrow 50 (* 1.5 pi)) 3 1))
                                      (scale-color 3 "black"))]
     )
  (slide
   #:name "CK+ Machine: Stack"
   #:title (make-title "CK+ Machine: Stack Structure")
   (comment 
    "The lazy abstract machine in our paper is called the CK+ machine, for the following reason. We reorganize the stack to be a stack of stacks, where there is always at least one substack in the stack. Each of these substacks has a bind continuation on top. This gives the machine easier access to all the arguments.\n\n"
    "Here is the control stack at the end of our previous example. Here are all the binds. In the CK+ machine, the control stack will look like this. You can see that there is now a stack of stacks, and each (except the first) has a bind continuation on top. I'll explain how the machine maintains this structure when I run through the example program with our machine in just a bit.")
   (item (hc-append (t "Reorganize stack to be ") (it "stack of stacks")))
   (subitem (tt "bind") " continuations on top")
   'next
   'alts
   (list
    (list
     (vc-append Ks-pict 
                (ghost connecting-down-arrow)
                (ghost K+s-pict-bold)))
    (list
     (vc-append Ks-pict-bold 
                (ghost connecting-down-arrow)
                (ghost K+s-pict-bold)))
    (list
     (vc-append Ks-pict-bold
                connecting-down-arrow
                K+s-pict-bold))
    )
   ))







;; CK+: Lexical Addresses Slide
(let*
    ([M-pict (tt "M = x | M M | λx.M")]
     [Mdb-pict (tt "M = n | M M | λ.M")]
     [K-pict (tt "K = mt | (arg M K) | (bind x M K) | (op x K K)")]
     [Kdb-pict (tt "K = mt | (arg M K) | (bind M K) | (op K K)")]
     [var1 (tt "0")]
     [var2 (tt "1")]
     [var3 (tt "0")]
     [term-pict (tt "λx.(x λy.(x y))")]
     [termdb-pict (hc-append lam1 DOT LPAR var1 SPC lam2 DOT LPAR var2 SPC var3 RPAR RPAR)]
     [termdb-arrows (lt-superimpose
                     (make-square-arrow-bot termdb-pict var1 lam1 20)
                     (make-square-arrow-bot termdb-pict var2 lam1 20)
                     (make-square-arrow-bot termdb-pict var3 lam2 40))]
     [slide-color-scale-factor 1.5]
     )
  (slide
   #:name "CK+ Machine: Lexical Addresses"
   #:title (make-title "CK+ Machine: Lexical Addresses")
   (comment "First, I want to show one more difference in our machine. In our machine we use lexical addresses to take advantage of our new stack structure. These are also called De Bruijn indices. Here are the changes to the machine terms. Variables become indices and lambdas no longer bind variables. Continuations no longer need to keep track of explicit variable names.\n\n"
            "As a quick example, a term like this gets transformed to a term like this, with indices. The indices count the number of lambdas between itself and its binding lambda and so the arrows here refer to the binding lambda for the variable.")
   (item "Replace variables with lexical addresses")
   (citation "De Bruijn 1972")
   'next
   'alts
   (list
    (list (vl-append 0 M-pict (ghost Mdb-pict)))
    (list (vl-append 0 (colorize M-pict 
                                 (scale-color slide-color-scale-factor "black")) 
                     Mdb-pict)))
   'next
   'alts
   (list
    (list (vl-append 0 K-pict (ghost Kdb-pict)))
    (list (vl-append 0 (colorize K-pict 
                                 (scale-color slide-color-scale-factor "black")) 
                     Kdb-pict)))
   'next
   'alts
   (list
    (list (vl-append 0 term-pict (ghost termdb-arrows) (ghost termdb-pict)))
    (list (vl-append 0 (colorize term-pict 
                                 (scale-color slide-color-scale-factor "black"))
                     termdb-pict (ghost termdb-arrows)))
    (list (vl-append 0 (colorize term-pict 
                                 (scale-color slide-color-scale-factor "black"))
                     termdb-pict termdb-arrows)))
 ))



;; CK+ Machine Example Slide
(let*
    ([K (list (list "(arg M)" "mt")
              (list "(bind M0)" "(arg M1)" "(arg M2)" "mt")
              (list "(bind M3)" "(arg M4)" "mt")
              (list "(bind M5)" "mt"))]
     [slide-font-size 17]
     [render-Ks (λ (Ks) (map (λ (lst) 
                               (vjoin-with-arrows
                                (map (λ (t) (tt t slide-font-size)) lst))) 
                             Ks))]
     [K-picts-lst (render-Ks K)]
     [K-picts (hjoin-with-arrows (render-Ks K))]
     [K-picts-lst-with-nums 
      (map (λ (x y) 
             (vc-append 
              x 
              (if (= y -1) ; dont give index to top partial frame
                  (blank)
                  (tt (number->string y)))))
           K-picts-lst
           (build-list (length K-picts-lst) sub1))]
     [var (tt "1")]
     [eq-pict (tt " = " slide-font-size)]
     [term-pict (tt "(λ.(λ.(λ.(1 M)) M0 M1 M2) M3 M4) M5")]
     [term-pict-chars (hc-append LPAR lam1 DOT LPAR lam2 DOT LPAR lam3 DOT LPAR var 
                                 SPC M RPAR RPAR SPC M0 SPC M1 SPC M2 RPAR SPC M3 SPC M4 RPAR SPC M5)]
     [term-pict-arrows (lb-superimpose
                   (make-square-arrow-top term-pict-chars lam1 M5 60)
                   (make-square-arrow-top term-pict-chars lam2 M3 40)
                   (make-square-arrow-top term-pict-chars lam3 M0 20))]
     [arrow-start (blank)]
     [make-step-pict-with-Kpicts
      (λ (term-pre term-underline term-post current-K-picts)
        (list (vc-append
               (vl-append
                term-pict-arrows
                (hc-append (tt term-pre)
                           (underline (tt term-underline))
                           (tt term-post)))
               (blank 0 (current-font-size))
               (vl-append
                (hc-append (btt "C") eq-pict 
                           (tt term-underline slide-font-size) arrow-start)
                (ht-append (btt "K") eq-pict 
                           (lt-superimpose
                            (hjoin-with-arrows current-K-picts)
                            (ghost K-picts)))))))]
     [make-step-pict
      (λ (term-pre term-underline term-post current-Ks)
        (make-step-pict-with-Kpicts term-pre term-underline term-post
                                    (render-Ks current-Ks)))]
     )
  (slide
   #:name "CK+ Machine: Example"
   #:title (make-title "CK+ Machine: Example")
   (comment 
    "Here is the same example as before but now it is being reduced by the CK+ machine. Here are the arguments and their corresponding lambdas again and again the contents of the C register will always be underlined. The main difference now is that we have a different stack structure with our stack of stacks. The rules that the machine follows is that frames only get pushed onto the top substack until a bind frame gets pushed onto the stack, at which point a new substack gets created. Here we have a lambda that is the operator of an application so we need to go under the lambda. The rule in our machine is that whenever we go under a lambda, we create a new substack. So here, the machine creates a bind continuation and starts a new substack in the control stack. We again push frames onto this new substack until we go under another lambda and start another new substack. Like before, we continue until we get to the variable, which is now an index.\n\n"
    "The point of having all those extra rules to arrange the stack like this is that when the machine reaches a variable index, it can use that index to immediately find its argument in the stack. Here I've numbered the substacks in the control stack. I've only numbered the substacks that have bind continuations on top. The bind that is numbered one conveniently contains the argument for variable 1.\n\n"
    "So what we've done here is use a static address to index into a dynamic control stack.")
   'alts
   (list
    (list (vl-append (ghost term-pict-arrows) term-pict))
    (list (vl-append term-pict-arrows term-pict))
    (make-step-pict 
     "" "(λ.(λ.(λ.(1 M)) M0 M1 M2) M3 M4) M5" ""
     (list (list "mt")))
    (make-step-pict 
     "" "(λ.(λ.(λ.(1 M)) M0 M1 M2) M3 M4)" " M5"
     (list (list "(arg M5)" "mt")))
    (make-step-pict 
     "(λ." "(λ.(λ.(1 M)) M0 M1 M2) M3 M4" ") M5"
     (list (list "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict 
     "(λ." "(λ.(λ.(1 M)) M0 M1 M2) M3" " M4) M5"
     (list (list "(arg M4)" "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict 
     "(λ." "(λ.(λ.(1 M)) M0 M1 M2)" " M3 M4) M5"
     (list (list "(arg M3)" "(arg M4)" "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict 
     "(λ.(λ." "(λ.(1 M)) M0 M1 M2" ") M3 M4) M5"
     (list (list "mt")
           (list "(bind M3)" "(arg M4)" "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict 
     "(λ.(λ." "(λ.(1 M)) M0 M1" " M2) M3 M4) M5"
     (list (list "(arg M2)" "mt")
           (list "(bind M3)" "(arg M4)" "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict 
     "(λ.(λ." "(λ.(1 M)) M0" " M1 M2) M3 M4) M5"
     (list (list "(arg M1)" "(arg M2)" "mt")
           (list "(bind M3)" "(arg M4)" "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict 
     "(λ.(λ." "(λ.(1 M))" " M0 M1 M2) M3 M4) M5"
     (list (list "(arg M0)" "(arg M1)" "(arg M2)" "mt")
           (list "(bind M3)" "(arg M4)" "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict 
     "(λ.(λ.(λ." "(1 M)" ") M0 M1 M2) M3 M4) M5"
     (list (list "mt")
           (list "(bind M0)" "(arg M1)" "(arg M2)" "mt")
           (list "(bind M3)" "(arg M4)" "mt")
           (list "(bind M5)" "mt")))
    (make-step-pict-with-Kpicts 
     "(λ.(λ.(λ.(" "1" " M)) M0 M1 M2) M3 M4) M5"
     K-picts-lst)
    (make-step-pict-with-Kpicts
     "(λ.(λ.(λ.(" "1" " M)) M0 M1 M2) M3 M4) M5"
     K-picts-lst-with-nums)
    (list
     (pin-arrow-line
      15
      (first (make-step-pict-with-Kpicts
              "(λ.(λ.(λ.(" "1" " M)) M0 M1 M2) M3 M4) M5"
              K-picts-lst-with-nums))
      arrow-start rc-find
      (list-ref K-picts-lst-with-nums 2) ct-find))
    ) ; end alt list
   'next
   (item #:fill? false "Direct index instead of search")
   ))







;; Stack Compact Slide
(let*
    ([slide-font-size 17]
     [tt (λ (txt (font-size slide-font-size)) (tt txt font-size))]   ; override
     [btt (λ (txt (font-size slide-font-size)) (btt txt font-size))] ; override
     [K1 (list (tt "(arg M)") (tt "mt"))]
     [K1-comp (list (tt "(arg M)") (tt "(arg M1)") (tt "(arg M2)") (tt "mt"))]
     [K2 (list (tt "(bind M0)") (tt "(arg M1)") (tt "(arg M2)") (tt "mt"))]
     [K2-bold (list (btt "(bind M0)") (tt "(arg M1)") (tt "(arg M2)") (tt "mt"))]
     [K3 (list (tt "(bind M3)") (tt "(arg M4)") (tt "mt"))]
     [K3-bold (list (btt "(bind M3)") (tt "(arg M4)") (tt "mt"))]
     [K4 (list (tt "(bind M5)") (tt "mt"))]
     [K4-bold (list (btt "(bind M5)") (tt "mt"))]
     [K= (hc-append (btt "K" TT-FONT-SIZE) (tt " = "))]
     [render-Ks (λ (Ks) (hjoin-with-arrows (map vjoin-with-arrows Ks)))]
     [init-Ks (render-Ks (list K1 K2 K3 K4))]
     [align-with-init (λ (Ks align-fn) (ht-append K= (align-fn Ks (ghost init-Ks))))]
     [make-alt (λ (Ks (align-fn cc-superimpose)) (list (align-with-init (render-Ks Ks) align-fn)))]
     [term-pict (tt "(λ.(λ.(λ.(1 M)) M0 M1 M2) M3 M4) M5" TT-FONT-SIZE)]
     )
  (slide
   #:name "Stack Compaction"
   #:title (make-title "Stack Compaction")
   (comment 
    "I'd like to talk about one more thing. One of the consequences of the call by need calculus is that arguments in an application are not removed. However, this is unacceptable in any real programming language implementation because the stack can grow unbounded. So another contribution of our paper is that we add stack compaction to our machine.\n\n"
    "The corresponding reduction in the calculus looks like this. It says that any arguments that are not used can be removed.\n\n"
    "The stack compaction algorithm in the machine will essentially apply the reduction repeatedly to arguments in the stack. Stack compaction is different from garbage collection because to determine whether an argument can be removed, you only have to check terms inside the lambda of the application. With garbage collection, you need to check the entire program.\n\n"
    "Here is the same example as before. I'm going to use it again to show how stack compaction in the CK+ machine works. Pretend that only the argument M3 is used in the program. Here is the control stack as before. For each binding, the stack compaction algorithm will check if any variables refer to the binding. If not, it will remove the binding. For each argument, since we only need to check terms under the lambda, that means that we only need to check the substacks to the left of the one with the argument, in addition to the contents of the C register.\n\n"
    "So first the stack checks M0 and sees that it is not used, so it removes it. It is important that the stack compaction maintain the special stack structure of the CK+ machine when an argument gets removed. Then the algorithm will check M3. Since M3 corresponds to the 1 variable, it is not removed. Finally, the algorithm checks the M5 bind and sees that it can also be removed.")
   'next
   (vc-append 0
              (hc-append (tt "((λx.M) N)" TT-FONT-SIZE) REDUCE-ARR (tt "M" TT-FONT-SIZE))
              (hc-append (t "where " MAIN-FONT-SIZE-SMALL) 
                         (tt "x " TT-FONT-SIZE) NOTIN (tt " FV(M)" TT-FONT-SIZE)))
   (blank)
   'next
   (vc-append 0
              term-pict 
              (t "where " MAIN-FONT-SIZE-SMALL) 
              (tt "No variables reference M0 or M5" TT-FONT-SIZE-SMALL))
   'next
   (lb-superimpose
    (hc-append (btt "C" TT-FONT-SIZE) (tt " = 1" slide-font-size))
    (ghost (ht-append K= init-Ks)))
   'alts
   (list
    (make-alt (list K1 K2 K3 K4))
    (make-alt (list K1 K2-bold K3 K4))
    (make-alt (list K1-comp K3 K4) rc-superimpose)
    (make-alt (list K1-comp K3-bold K4) rc-superimpose)
    (make-alt (list K1-comp K3 K4-bold) rc-superimpose)
    (make-alt (list K1-comp K3))

    )
   ))





;; Thanks! Slide
(slide
 #:name "Thanks"
 (t "Thanks!" 36))