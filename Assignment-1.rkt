#lang racket
; TO DO
; - load rackunit from cmd line
; - Change ifleq0 to (M0 <= 0 ? M1 : M2)
(require rackunit)
(require racket/cmdline)


#|
Template
LC = num
   | id
   | (λ (id) LC)       ==>  function (id) { LC }
   | (LC LC)           ==> LC(LC);
   | (+ LC LC)         ==> LC + LC
   | (* LC LC)         ==> LC * LC
   | (ifleq0 LC1 LC2 LC3) ==>  LC1 <= 0 ? LC2 : LC3 | OLD =>if (LC) {LC} else {LC}
   | (println LC)      ==> console.log(LC);

|#

;; Translates LC expr into JS 
#|
Template
...(cond ...
...[(number? s) ...]
...[(symbol? s) ...]
...[(list ? s) ...]
|#
(define (translate s)
  (cond
    [(number? s) s]
    [(symbol? s) s]
    [(list? s)
     (define s-list s) 
     (match s-list 
       [(list 'λ (? list? id) body)
        (define translated-body (translate body))
       (append `(function ,id) '(|{|) '(return) `(,translated-body) '(|}|))]
       [(list '+ lhs rhs) 
        (define translated-lhs (translate lhs))
        (define translated-rhs (translate rhs))
        `(,translated-lhs + ,translated-rhs)]
       [(list '* lhs rhs)
        (define translated-lhs (translate lhs))
        (define translated-rhs (translate rhs))
        `(,translated-lhs * ,translated-rhs)]
       [(list 'ifleq0 test then else)
        (define translated-test (translate test))
        (define new-translated-test
          (cond
            [(list? translated-test)
             translated-test]
            [else (cons translated-test '())]))
        (define translated-then (translate then))
        (define new-translated-then
          (cond
            [(list? translated-then)
             translated-then]
            [else (cons translated-then '())]))
        (define translated-else (translate else))
        (define new-translated-else
          (cond
            [(list? translated-else)
             translated-else]
            [else (cons translated-else '())]))
        (define new-else (cons translated-else '()))
   #| OLD if else
        (append `(if (,@new-translated-test <= 0))
                '(|{|) `(,@new-translated-then) '(|}|)
                '(else) '(|{|) `(,@new-translated-else) '(|}|))
   |#
       `(,@new-translated-test <= 0 ? ,@new-translated-then : ,@new-translated-else)
        ] 
       [(list 'println message)
        (define translated-message (translate message))
        (define new-translated-message
          (cond
            [(list? translated-message)
             translated-message] 
            [else (cons translated-message '())]))
        `(console.log(,@new-translated-message))]
       [(list func args)
        (define translated-func (translate func))
        (define new-translated-func
          (cond
            [(list? translated-func)
             translated-func]
            [else (cons translated-func '())]))
        (define translated-args (translate args))
        (define new-translated-args
          (cond
            [(list? translated-args)
             translated-args]
            [else (cons translated-args '())]))
        `(((,@new-translated-func) (,@new-translated-args)))]
       )] 
    [else (error 'translate "LC: Sexp not part of target language.")]))

(define (serialize s) 
  (cond 
    [(number? s) (~a s)]
    [(symbol? s) (~a s)]
    [else
     (define enclosed-sexp(~a s))
     (define enclosed-sexp-len (string-length enclosed-sexp))
     (substring enclosed-sexp 1 (- enclosed-sexp-len 1))]))

(define (top-translate s) 
  (serialize (translate s)))


;;;;;;;;;;;;;;;;;;;
;; PROGRAM START ;;
;;;;;;;;;;;;;;;;;;;
#|
;; Get input file and send to top-translate then output to .js file
(define file_in (command-line #:args (filename_in filename_out) filename_in))
(define file_out (command-line #:args (filename_in filename_out) filename_out))
(define file_out_port (open-output-file file_out #:exists 'replace))
(define to_translate_port (open-input-file file_in #:mode 'text))
(define to_translate (read to_translate_port))
(define translated_prog (top-translate to_translate))
(printf "Target Program: ")
(print to_translate)
(printf "\n" )
(printf "JS version: ")
(print translated_prog)
(printf "\n" )
;; Send translated_prog to output file
(fprintf file_out_port translated_prog)
|#
 
;;;;;;;;;;;
;; TESTS ;;
;;;;;;;;;;;

;; Translate Tests

(check-equal? (translate 123) 123)
(check-equal? (translate 'a) 'a) 
(check-equal? (translate '(λ (x) do_crap)) '(function (x) |{| return do_crap |}|))
(check-equal? (translate '(λ (x) (* 2 2))) '(function (x) |{| return (2 * 2) |}|))
(check-equal? (translate '(+ 1 2)) '(1 + 2))
(check-equal? (translate '(* 1 2)) '(1 * 2))
(check-equal? (translate '(ifleq0 0 1 2)) '(0 <= 0 ? 1 : 2))
(check-equal? (translate '(ifleq0 (+ 0 1) (+ 0 2) (+ 0 3))) '(0 + 1 <= 0 ? 0 + 2 : 0 + 3))
(check-equal? (translate '(println something)) '(console.log(something)))
(check-equal? (translate '(println (+ 1 2))) '(console.log(1 + 2)))
(check-equal? (translate '(func_name 2)) '(((func_name) (2))))
(check-equal? (translate '(func_name (+ 1 2))) '(((func_name) (1 + 2))))
(check-equal? (translate '((λ (x) (+ x 3)) 7)) '(((function (x) |{| return (x + 3) |}|) (7))))
(check-exn (regexp (regexp-quote "LC: Sexp not part of target language."))
           (lambda () (translate "something")))
  
 
;; Top-Translate Tests
(check-equal? (top-translate 123) "123")
(check-equal? (top-translate 'a) "a")
(check-equal? (top-translate '(λ (x) do_crap)) "function (x) { return do_crap }")
(check-equal? (top-translate '(λ (x) (* 2 2))) "function (x) { return (2 * 2) }")
(check-equal? (top-translate '(+ 1 2)) "1 + 2")
(check-equal? (top-translate '(* 1 2)) "1 * 2")
(check-equal? (top-translate '(+ (+ 1 2) (* 2 3))) "(1 + 2) + (2 * 3)")
(check-equal? (top-translate '(ifleq0 0 1 2)) "if (0 <= 0) { 1 } else { 2 }")
(check-equal? (top-translate '(println something)) "console.log (something)")
(check-equal? (top-translate '(func_app 2)) "((func_app) (2))")
(check-equal? (top-translate '(func_app (+ 1 2))) "((func_app) (1 + 2))")
(check-equal? (top-translate '((λ (x) (+ x 3)) 7)) "((function (x) { return (x + 3) }) (7))")

