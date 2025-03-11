;;; Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2)
;;; Desenvolvido por:
;;; Jonatas Dias Machado Costa (202165077AC)
;;; Maria Luísa Riolino Guimarães (202165563C)

#lang racket
(require dcc019/exercise/logic/ast)

(provide eval-query)

;; ---------------------------
;; Representação do Programa (Base de Conhecimento)
;; ---------------------------
(define knowledge-base '())

(define (load-program prog)
  (displayln "[DEBUG] Carregando programa na base de conhecimento...")
  (set! knowledge-base (if (list? prog) prog (list prog))))

;; ---------------------------
;; Função auxiliar: converte uma consulta (que pode ser um improper list) em uma lista de goals
;; ---------------------------
(define (goals->list q)
  (cond
    [(null? q) '()]
    [(pair? q) (cons (car q) (goals->list (cdr q)))]
    [else (list q)]))

;; ---------------------------
;; Funções de Freshening (Renomeação) para Cláusulas (regras)
;; ---------------------------
(define (rename-term term env)
  (cond
    [(ast:var? term)
     (let ([binding (assoc term env)])
       (if binding
           (values (cdr binding) env)
           (let ([new-var (ast:var (gensym (ast:var-name term)))])
             (values new-var (cons (cons term new-var) env)))))]
    [(ast:atom? term) (values term env)]
    [(functor? term)
     (let* ([name (functor-name term)]
            [args (functor-args term)])
       (call-with-values
        (lambda () (rename-term-list args env))
        (lambda (new-args new-env)
          (values (ast:functor name new-args) new-env))))]
    [else (values term env)]))

(define (rename-term-list terms env)
  (if (null? terms)
      (values '() env)
      (call-with-values
       (lambda () (rename-term (car terms) env))
       (lambda (new-term env1)
         (call-with-values
          (lambda () (rename-term-list (cdr terms) env1))
          (lambda (new-terms env2)
            (values (cons new-term new-terms) env2)))))))

(define (rename-clause clause)
  (call-with-values
   (lambda () (rename-term (clause-head clause) '()))
   (lambda (new-head env1)
     (call-with-values
      (lambda () (rename-term-list (clause-body clause) env1))
      (lambda (new-body env2)
        (ast:clause new-head new-body))))))

;; ---------------------------
;; Função Principal: Avaliar Consulta
;; ---------------------------
(define (eval-query program query)
  (load-program program)
  (displayln (format "[DEBUG] Iniciando resolução para consulta: ~a" query))
  (let* ([goals (if (pair? query)
                    (goals->list query)
                    (list query))]
         [results (if (null? goals)
                      '()
                      (resolve-query-body goals '()))])
    (if (null? results)
        (begin 
          (displayln "[DEBUG] Consulta falhou.")
          (display "False\n")
          #'(void))
        (begin
          (displayln "[DEBUG] Consulta bem-sucedida, exibindo soluções...")
          (display "True\n\n")
          (for-each (lambda (env)
                      (display-env env)
                      (newline))
                    results)
          #'(void)))))

;; ---------------------------
;; Resolução de Consultas (Goals)
;; ---------------------------
(define (resolve-query query env)
  (displayln (format "[DEBUG] Resolvendo consulta: ~a" query))
  (if (null? knowledge-base)
      (begin (displayln "[DEBUG] Base de conhecimento vazia!") '())
      (search-rules query env knowledge-base)))

(define (resolve-query-body goals env)
  (displayln (format "[DEBUG] Resolvendo consulta composta: ~a" goals))
  (if (null? goals)
      (list env)
      (let* ([first-goal (car goals)]
             [first-results (search-rules first-goal env knowledge-base)])
        (apply append (map (lambda (env1)
                             (resolve-query-body (cdr goals) env1))
                           first-results)))))

;; ---------------------------
;; Busca por Fatos e Regras
;; ---------------------------
(define (search-rules query env rules)
  (displayln (format "[DEBUG] Buscando regras para: ~a" query))
  (if (null? rules)
      (begin (displayln "[DEBUG] Nenhuma regra correspondente encontrada.") '())
      (let* ([rule (car rules)]
             [head (clause-head rule)]
             [body (clause-body rule)]
             [unif (unify query head env)]
             [body-goals (goals->list body)]) ; Garante que o corpo seja uma lista de subgoals
        (displayln (format "[DEBUG] Testando regra: ~a" head))
        (if unif
            (let ([result (if (null? body-goals)
                              (list unif)
                              (resolve-query-body body-goals unif))])
              (displayln "[DEBUG] Unificação bem-sucedida.")
              (append result (search-rules query env (cdr rules))))
            (search-rules query env (cdr rules))))))

;; ---------------------------
;; Unificação de Termos
;; ---------------------------
(define (walk term env)
  (if (and (ast:var? term) (assoc term env))
      (walk (cdr (assoc term env)) env)
      term))

(define (unify term1 term2 env)
  (displayln (format "[DEBUG] Tentando unificar: ~a com ~a" term1 term2))
  (let* ([t1 (walk term1 env)]
         [t2 (walk term2 env)])
    (cond
      [(equal? t1 t2) (displayln "[DEBUG] Termos idênticos.") env]
      [(ast:var? t1) (bind-variable t1 t2 env)]
      [(ast:var? t2) (bind-variable t2 t1 env)]
      [(and (functor? t1) (functor? t2))
       (if (equal? (functor-name t1) (functor-name t2))
           (unify-args (functor-args t1) (functor-args t2) env)
           (begin (displayln "[DEBUG] Functores diferentes, falha na unificação.") #f))]
      [else (displayln "[DEBUG] Tipos incompatíveis para unificação.") #f])))

(define (unify-args args1 args2 env)
  (displayln "[DEBUG] Unificando argumentos...")
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

;; ---------------------------
;; Associação de Variáveis no Ambiente
;; ---------------------------
(define (bind-variable var value env)
  (let ([binding (assoc var env)])
    (if binding
        (if (equal? (cdr binding) value)
            env
            #f)
        (begin
          (displayln (format "[DEBUG] Associando variável ~a = ~a" var value))
          (cons (cons var value) env)))))

;; ---------------------------
;; Impressão do Ambiente (Melhorada)
;; ---------------------------
(define (display-env env)
  (for-each
   (lambda (binding)
     (displayln (format "~a = ~a" (extract-name (car binding))
                               (extract-name (cdr binding)))))
   env))

(define (extract-name term)
  (cond
    [(ast:var? term) (ast:var-name term)]
    [(ast:atom? term) (ast:atom-name term)]
    [else term]))

;; ---------------------------
;; Predicados Auxiliares
;; ---------------------------
(require (prefix-in ast: dcc019/exercise/logic/ast))
(define (var? x) (ast:var? x))
(define (functor? x) (ast:functor? x))
(define (clause-head c) (ast:clause-head c))
(define (clause-body c) (ast:clause-body c))
(define (functor-name f) (ast:functor-name f))
(define (functor-args f) (ast:functor-args f))
