;;; Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2)
;;; Desenvolvido por:
;;; Jonatas Dias Machado Costa (202165077AC)
;;; Maria Luísa Riolino Guimarães (202165563C)

#lang racket

(require dcc019/exercise/logic/ast)
(require dcc019/exercise/logic/lang/reader)

(provide eval-query)

;; ---------------------------
;; Representação do Programa (Base de Conhecimento)
;; ---------------------------
(define knowledge-base '()) ;; Lista global para armazenar as cláusulas

;; Função para carregar o programa na base de conhecimento
(define (load-program prog)
  (set! knowledge-base prog))

;; ---------------------------
;; Função Principal: Avaliar Consulta
;; ---------------------------
(define (eval-query program query)
  (load-program program) ;; Carregar base de conhecimento
  (resolve-query query '())) ;; Resolver a consulta com ambiente inicial vazio

;; ---------------------------
;; Resolver a Consulta
;; ---------------------------
(define (resolve-query query env)
  (cond
    [(null? knowledge-base) 'fail] ;; Se não há regras, falha
    [else (search-rules query env knowledge-base)])) ;; Buscar regras compatíveis

;; ---------------------------
;; Busca por Fatos e Regras
;; ---------------------------
(define (search-rules query env rules)
  (cond
    [(null? rules) 'fail] ;; Se nenhuma regra se aplica, falha
    [else
     (let* ([rule (car rules)]
            [head (clause-head rule)]
            [body (clause-body rule)]
            [unif (unify query head env)])
       (if unif
           (if (null? body)
               'success ;; Se é um fato, sucesso imediato
               (resolve-query body unif)) ;; Se é uma regra, resolver o corpo
           (search-rules query env (cdr rules))))]))

;; ---------------------------
;; Unificação de Termos
;; ---------------------------
(define (unify term1 term2 env)
  (cond
    [(equal? term1 term2) env] ;; Se são iguais, sucesso
    [(var? term1) (bind-variable term1 term2 env)] ;; Se é variável, tenta unificar
    [(var? term2) (bind-variable term2 term1 env)]
    [(and (functor? term1) (functor? term2) (equal? (functor-name term1) (functor-name term2)))
     (unify-args (functor-args term1) (functor-args term2) env)]
    [else #f])) ;; Falha se não puder unificar

;; ---------------------------
;; Unificação de Argumentos (para Functores)
;; ---------------------------
(define (unify-args args1 args2 env)
  (if (null? args1)
      env
      (let ([new-env (unify (car args1) (car args2) env)])
        (if new-env
            (unify-args (cdr args1) (cdr args2) new-env)
            #f)))) ;; Falha se qualquer argumento falhar

;; ---------------------------
;; Função para associar uma variável a um valor no ambiente
;; ---------------------------
(define (bind-variable var value env)
  (cons (cons var value) env))

;; ---------------------------
;; Predicados Auxiliares
;; ---------------------------
(define (var? x) (struct? x 'var))
(define (functor? x) (struct? x 'functor))
(define (clause-head c) (clause-head c))
(define (clause-body c) (clause-body c))

;; Extrai o nome do functor
(define (functor-name f) (functor-name f))

;; Extrai os argumentos do functor
(define (functor-args f) (functor-args f))
