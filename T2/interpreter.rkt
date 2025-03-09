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
  (set! knowledge-base prog))

;; ---------------------------
;; Função Principal: Avaliar Consulta (Versão Corrigida)
;; ---------------------------
(define (eval-query program query)
  (load-program program)
  (let ([result (resolve-query query '())])
    (if result
        (begin
          (display "True") (newline)
          (display-env result) #'(void)) ; Exibe o ambiente de unificação
        (begin (display "False") (newline) #'(void)))))

;; ---------------------------
;; Resolver a Consulta (Atualizada)
;; ---------------------------
(define (resolve-query query env)
  (cond
    [(null? knowledge-base) #f]
    [else 
     (search-rules query env knowledge-base)])) ; Retorna o ambiente encontrado

;; ---------------------------
;; Funções auxiliares para impressão no terminal
;; ---------------------------
(define (display-env env)
  (for-each
   (lambda (binding)
     (let* ([var (car binding)]
            [value (cdr binding)]
            [var-name (extract-name var)]
            [value-name (extract-name value)])
       (display var-name) (display " = ") (display value-name)))
   env))

(define (extract-name term)
  (cond
    [(ast:var? term) (ast:var-name term)]   ; Se for variável, pega o nome
    [(ast:atom? term) (ast:atom-name term)] ; Se for átomo, pega o nome
    [else term])) ; Caso contrário, retorna o próprio valor



;; ---------------------------
;; Busca por Fatos e Regras (Atualizada)
;; ---------------------------
(define (search-rules query env rules)
  (cond
    [(null? rules) #f]
    [else
     (let* ([rule (car rules)]
            [head (clause-head rule)]
            [body (clause-body rule)]
            [unif (unify query head env)]) ; unify retorna #f ou ambiente
       (if unif
           (if (null? body)
               unif ; Retorna ambiente se for fato
               (let ([body-result (resolve-query-body body unif)])
                 (if body-result 
                     body-result ; Retorna ambiente se corpo OK
                     (search-rules query env (cdr rules))))) ; Falha no corpo, próxima regra
           (search-rules query env (cdr rules))))])) ; Falha na unificação, próxima regra

;; ---------------------------
;; Resolução do Corpo da Regra (Atualizada)
;; ---------------------------
(define (resolve-query-body body env)
  (cond
    [(null? body) env] ; Sucesso retorna ambiente
    [else
     (let ([result (resolve-query (car body) env)])
       (if result 
           (resolve-query-body (cdr body) env) ; Continua se subgoal OK
           #f))])) ; Falha se algum subgoal falhar

;; ---------------------------
;; Unificação de Termos (Modificado com mais logs)
;; ---------------------------
(define (unify term1 term2 env)
  (cond
    [(equal? term1 term2) env]
    [(var? term1) (bind-variable term1 term2 env)]
    [(var? term2) (bind-variable term2 term1 env)]
    [(and (functor? term1) (functor? term2))
     (if (equal? (functor-name term1) (functor-name term2))
         (unify-args (functor-args term1) (functor-args term2) env)
         #f)]
    [else #f]))

;; ---------------------------
;; Unificação de Argumentos (Corrigido para improper lists)
;; ---------------------------
(define (unify-args args1 args2 env)
  (cond
    [(and (null? args1) (null? args2)) env]
    [(and (pair? args1) (pair? args2))
     (let ([new-env (unify (car args1) (car args2) env)])
       (if new-env
           (unify-args (cdr args1) (cdr args2) new-env)
           #f))]
    [(and (not (null? args1)) (not (null? args2)))
     (unify args1 args2 env)]
    [else #f]))

;; ---------------------------
;; Função para associar variável (Modificado com mais logs)
;; ---------------------------
(define (bind-variable var value env)
  (cons (cons var value) env))

;; ---------------------------
;; Predicados Auxiliares (Corrigido)
;; ---------------------------
(require (prefix-in ast: dcc019/exercise/logic/ast)) ; Importar com prefixo

(define (var? x) (ast:var? x))
(define (functor? x) (ast:functor? x))
(define (clause-head c) (ast:clause-head c)) ; Usar acessores do módulo ast
(define (clause-body c) (ast:clause-body c))

(define (functor-name f) (ast:functor-name f))
(define (functor-args f) (ast:functor-args f))