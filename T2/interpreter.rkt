;;; Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2)
;;; Desenvolvido por:
;;; Jonatas Dias Machado Costa (202165077AC)
;;; Maria Luísa Riolino Guimarães (202165563C)

#lang racket
(require dcc019/exercise/logic/ast)

(provide eval-query)

;; Representação do programa (base de conhecimento)
(define knowledge-base '())

;; Carrega o programa (fatos e regras) na base de conhecimento
(define (load-program prog)
  (set! knowledge-base (if (list? prog) prog (list prog))))

;; ---------------------------------------------------------
;; FUNÇÃO PRINCIPAL: recebe e avalia a consulta / retorna resultado(s)
;; ---------------------------------------------------------

(define (eval-query program query)
  (load-program program)
  (let* ([goals (if (pair? query)
                    (goals->list query)
                    (list query))]
         [results (if (null? goals)
                      '()
                      (resolve-query-body goals '()))])
    (if (null? results)
        (begin 
          (display "False\n")
          #'(void))
        (begin
          (display "True\n\n")
          (for-each (lambda (env)
                      (display-env env)
                      (newline))
                    results)
          #'(void)))))

;; ---------------------------------------------------------
;; RESOLUÇÃO DAS CONSULTAS
;; ---------------------------------------------------------

;; resolve cada subgoal
(define (resolve-query-body goals env)
  (cond
    [(null? goals) (list env)]
    [(ast:neq? (car goals))
     (if (check-diff env (ast:neq-lterm (car goals)) (ast:neq-rterm (car goals)))
         (resolve-query-body (cdr goals) env)
         '())]
    [else
     (append-map (lambda (env1) 
                   (resolve-query-body (cdr goals) env1))
                 (search-rules (car goals) env knowledge-base))]))

;; busca por fatos e regras na base de conhecimento
(define (search-rules query env rules)
  (if (null? rules)
      '()
      (let* ([rule (car rules)]
             [renamed-rule (rename-clause rule)]
             [unif (unify query (clause-head renamed-rule) env)]
             [body-goals (goals->list (clause-body renamed-rule))])
        (append (if unif (resolve-query-body body-goals unif) '()) 
                (search-rules query env (cdr rules))))))

;; substitui variável por seu valor no ambiente (env)
(define (walk term env)
  (if (and (ast:var? term) (assoc term env))
      (walk (cdr (assoc term env)) env)
      term))

;; UNIFICAÇÃO
(define (unify term1 term2 env)
  (let* ([t1 (walk term1 env)]
         [t2 (walk term2 env)])
    (cond
      [(equal? t1 t2) env]                          ; termos iguais -> retorna env
      [(ast:var? t1) (bind-variable t1 t2 env)]     ; um termo var -> associa ao outro termo
      [(ast:var? t2) (bind-variable t2 t1 env)]     
      [(and (ast:functor? t1) (ast:functor? t2))    
       (if (equal? (ast:functor-name t1) (ast:functor-name t2))
           (unify-args (ast:functor-args t1) (ast:functor-args t2) env)
           #f)]                                     ; functores -> compara nomes e unifica
      [else #f])))

(define (unify-args args1 args2 env)
  (let* ([lst1 (goals->list args1)]
         [lst2 (goals->list args2)])
    (cond
      [(and (null? lst1) (null? lst2)) env]
      [(and (pair? lst1) (pair? lst2))
       (let ([new-env (unify (car lst1) (car lst2) env)])
         (if new-env
             (unify-args (cdr lst1) (cdr lst2) new-env)
             #f))]
      [else #f])))

;; DESIGUALDADE
(define (check-diff env term1 term2)
  (let* ([val1 (walk term1 env)]
         [val2 (walk term2 env)])
    (not (terms-equal? val1 val2))))

(define (terms-equal? t1 t2)
  (let ([t1 (walk t1 '())]            ; final -> força caminhada completa sem env
        [t2 (walk t2 '())])
    (cond
      [(and (ast:var? t1) (ast:var? t2)) (eq? t1 t2)]
      [(and (ast:functor? t1) (ast:functor? t2))
       (and (equal? (ast:functor-name t1) (ast:functor-name t2))
            (andmap terms-equal? 
                    (ast:functor-args t1) 
                    (ast:functor-args t2)))]
      [else (equal? t1 t2)])))

;; ---------------------------------------------------------
;; Funções Auxiliares
;; ---------------------------------------------------------

;; (AUX) Converte a consulta em uma lista de objetivos
(define (goals->list q)
  (cond
    [(null? q) '()]
    [(pair? q) (cons (car q) (goals->list (cdr q)))]
    [else (list q)]))

;; (AUX) Associa variáveis no ambiente
(define (bind-variable var value env)
  (if (assoc var env)
      env
      (cons (cons var value) env)))

;; (AUX) Renomeia as variáveis em cláusulas quando necessário
(define (rename-term term env)
  (cond
    [(ast:var? term)
     (let ([binding (assoc term env)])
       (if binding
           (values (cdr binding) env)
           (let ([new-var (ast:var (gensym (ast:var-name term)))])
             (values new-var (cons (cons term new-var) env)))))]
    [(ast:unknow-var? term)
     (let ([new-var (ast:var (gensym '_))])
       (values new-var env))]
    [(ast:atom? term) (values term env)]
    [(ast:functor? term)
     (let-values ([(new-args new-env) (rename-term-list (ast:functor-args term) env)])
       (values (ast:functor (ast:functor-name term) new-args) new-env))]
    [else (values term env)]))

;; (AUX) Renomeia lista de termos
(define (rename-term-list terms env)
  (cond
    [(null? terms) 
     (values '() env)]
    [(pair? terms)
     (call-with-values
      (lambda () (rename-term (car terms) env))
      (lambda (new-term env1)
        (call-with-values
         (lambda () (rename-term-list (cdr terms) env1))
         (lambda (new-terms env2)
           (values (cons new-term new-terms) env2)))))]
    [else
     (call-with-values
      (lambda () (rename-term terms env))
      (lambda (new-term env1)
        (values (list new-term) env1)))]))

;; (AUX) Renomeia cabeça e corpo das cláusulas
(define (rename-clause clause)
  (call-with-values
   (lambda () (rename-term (clause-head clause) '()))
   (lambda (new-head env1)
     (call-with-values
      (lambda () (rename-term-list (clause-body clause) env1))
      (lambda (new-body env2)
        (ast:clause new-head new-body))))))

;; (AUX) Imprime o ambiente (env)
(define (display-env env)
  (for-each
   (lambda (binding)
     (let* ([var (car binding)]
            [value (cdr binding)]
            ;; extrai nomes base das variáveis
            [var-name (extract-name var)]
            [value-name (extract-name value)])
       (display (format "~a = ~a\n" var-name value-name))))
   env))

(define (extract-name term)
  (cond
    [(ast:var? term)
     ;; converte para string se for símbolo ou mantém a string original
     (let* ([name (ast:var-name term)]
            [name-str (if (symbol? name) 
                          (symbol->string name) 
                          name)]
            [base-name (car (regexp-match #px"^([A-Za-z]+)" name-str))])
       (string->symbol base-name))]
    [(ast:atom? term) (ast:atom-name term)]
    [else term]))

;; (AUX) Predicados
(require (prefix-in ast: dcc019/exercise/logic/ast))
(define (var? x) (ast:var? x))
(define (functor? x) (ast:functor? x))
(define (clause-head c) (ast:clause-head c))
(define (clause-body c) (ast:clause-body c))
(define (functor-name f) (ast:functor-name f))
(define (functor-args f) (ast:functor-args f))