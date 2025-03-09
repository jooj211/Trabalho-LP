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
  (displayln (format "\n>>> Carregando programa na base de conhecimento <<<"))
  (set! knowledge-base prog)
  (displayln "Programa carregado com sucesso!")
  (displayln "Cláusulas carregadas:")
  (for ([clause prog])
    (displayln (format "- ~a" clause))))

;; ---------------------------
;; Função Principal: Avaliar Consulta (Versão Corrigida)
;; ---------------------------
(define (eval-query program query)
  (displayln "\n=== INICIANDO AVALIAÇÃO DA CONSULTA ===")
  (load-program program)
  (displayln (format "\n>>> Iniciando resolução para consulta: ~a <<<" query))
  (if (resolve-query query '()) #'#t #'#f)) ; Retorna booleano

;; ---------------------------
;; Resolver a Consulta (Atualizada)
;; ---------------------------
(define (resolve-query query env)
  (displayln (format "Consultando: ~a com ambiente: ~a" query env))
  (cond
    [(null? knowledge-base) #f] 
    [else 
     (let ([result (search-rules query env knowledge-base)])
       ;; Alterar para verificar result
       (if result
           #t 
           #f))])) ; Só retorna #t se houver ambiente válido


;; ---------------------------
;; Busca por Fatos e Regras (Atualizada)
;; ---------------------------
(define (search-rules query env rules)
  (cond
    [(null? rules) 
     (displayln "!!! Nenhuma regra restante !!!")
     #f]
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
  (displayln (format "\n[UNIFICACAO] Comparando:"))
  (displayln (format "Termo1: ~a" term1))
  (displayln (format "Termo2: ~a" term2))
  (displayln (format "Ambiente: ~a" env))
  
  (cond
    [(equal? term1 term2)
     (displayln "[SUCESSO] Termos idênticos")
     env]
    
    [(var? term1)
     (displayln (format "[VARIAVEL] Vinculando ~a = ~a" term1 term2))
     (bind-variable term1 term2 env)]
    
    [(var? term2)
     (displayln (format "[VARIAVEL] Vinculando ~a = ~a" term2 term1))
     (bind-variable term2 term1 env)]
    
    [(and (functor? term1) (functor? term2))
     (displayln "[FUNCTOR] Ambos termos são functores")
     (if (equal? (functor-name term1) (functor-name term2))
         (begin
           (displayln (format "[FUNCTOR] Mesmo nome: ~a" (functor-name term1)))
           (unify-args (functor-args term1) (functor-args term2) env))
         (begin
           (displayln (format "[FALHA] Nomes diferentes: ~a vs ~a" 
                            (functor-name term1) (functor-name term2)))
           #f))]
    
    [else
     (displayln "[FALHA] Tipos incompatíveis para unificação")
     #f]))

;; ---------------------------
;; Unificação de Argumentos (Corrigido para improper lists)
;; ---------------------------
(define (unify-args args1 args2 env)
  (displayln (format "\nUnificando argumentos:"))
  (displayln (format "Argumentos1: ~a" args1))
  (displayln (format "Argumentos2: ~a" args2))
  
  (cond
    ;; Caso base: ambos os argumentos são null (listas próprias)
    [(and (null? args1) (null? args2))
     (displayln "Todos argumentos processados com sucesso")
     env]
    
    ;; Verifica se ambos são pares ou listas
    [(and (pair? args1) (pair? args2))
     (let ([arg1 (car args1)]
           [arg2 (car args2)])
       (displayln (format "\nUnificando par de argumentos:"))
       (displayln (format "- Primeiro: ~a" arg1))
       (displayln (format "- Segundo: ~a" arg2))
       
       (let ([new-env (unify arg1 arg2 env)])
         (if new-env
             (begin
               (displayln "Par unificado com sucesso")
               (unify-args (cdr args1) (cdr args2) new-env))
             (begin
               (displayln "!!! Falha na unificação de argumentos !!!")
               #f))))]
    
    ;; Caso para improper lists (último elemento)
    [(and (not (null? args1)) (not (null? args2)))
     (displayln (format "\nUnificando último par de argumentos:"))
     (displayln (format "- Primeiro: ~a" args1))
     (displayln (format "- Segundo: ~a" args2))
     (unify args1 args2 env)]
    
    ;; Número de argumentos diferente
    [else
     (displayln "!!! Número de argumentos incompatível !!!")
     #f]))

;; ---------------------------
;; Função para associar variável (Modificado com mais logs)
;; ---------------------------
(define (bind-variable var value env)
  (displayln (format "\nVinculando variável ~a a ~a" var value))
  (displayln (format "Ambiente antes da vinculação: ~a" env))
  (let ([new-env (cons (cons var value) env)])
    (displayln (format "Novo ambiente: ~a" new-env))
    new-env))

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
