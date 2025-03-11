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
  (set! knowledge-base prog)
  (displayln "[DEBUG] Cláusulas carregadas:")
  (for ([clause prog] [i (in-naturals 1)])
    (displayln (format "  [Regra ~a] ~a" i clause))))

;; ---------------------------
;; Função Principal: Avaliar Consulta (Versão Corrigida)
;; ---------------------------
(define (eval-query program query)
  (load-program program)
  (displayln (format "\n[DEBUG] Iniciando resolução para consulta: ~a" query))
  (let ([results (resolve-query query '())])  ;; Agora pode retornar múltiplas soluções
    (if (null? results)
        (begin 
          (display "False\n\n") #'(void))
        (begin
          (display "True\n\n")
          (for-each (lambda (env)
                      (display-env env)
                      (newline))
                    results)
          (newline) #'(void)))))


;; ---------------------------
;; Resolver a Consulta (Atualizada)
;; ---------------------------
(define (resolve-query query env)
  (displayln (format "[DEBUG] Resolvendo consulta: ~a" query))
  (cond
    [(null? knowledge-base) 
     (displayln "[DEBUG] Base de conhecimento vazia!")
     #f]
    [else 
     (search-rules query env knowledge-base)]))

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
    [(ast:var? term) (ast:var-name term)]
    [(ast:atom? term) (ast:atom-name term)]
    [else term]))

;; ---------------------------
;; Busca por Fatos e Regras (Atualizada)
;; ---------------------------
(define (search-rules query env rules)
  (displayln (format "[DEBUG] Buscando em ~a regra(s)" (length rules)))
  (cond
    [(null? rules) 
     (displayln "[DEBUG] Nenhuma regra restante!")
     '()]  ;; Retorna uma lista vazia
    [else
     (let* ([rule (car rules)]
            [head (clause-head rule)]
            [body (clause-body rule)]
            [unif (unify query head env)])
       (displayln (format "[DEBUG] Testando regra: ~a" head))
       (if unif
           (let ([result (if (null? body)
                             (list unif)  ;; Se for fato, retorna lista com solução
                             (resolve-query-body body unif))])
             (append result (search-rules query env (cdr rules))))
           (search-rules query env (cdr rules))))]))



;; ---------------------------
;; Resolução do Corpo da Regra (Versão Final Corrigida)
;; ---------------------------
(define (resolve-query-body body env)
  (displayln (format "[DEBUG] Resolvendo corpo da regra: ~a" body))
  (cond
    [(not (pair? body)) 
     (displayln "[DEBUG] Processando último subgoal")
     (resolve-query body env)]  
    [else
     (let ([first-goal (car body)])
       (displayln (format "[DEBUG] Processando subgoal: ~a" first-goal))
       (let ([results (resolve-query first-goal env)])  ;; Agora pode retornar múltiplas soluções
         (if (null? results)
             '()  ;; Se falhar, não há soluções
             (apply append (map (lambda (new-env)  ;; Gera combinações de soluções
                                  (resolve-query-body (cdr body) new-env))
                                results)))))]))  


;; ---------------------------
;; Unificação de Termos (Modificado com mais logs)
;; ---------------------------
(define (unify term1 term2 env)
  (displayln (format "[DEBUG] Tentando unificar: ~a com ~a" 
                   term1
                   term2))
  (cond
    [(equal? term1 term2) 
     (displayln "[DEBUG] Termos idênticos")
     env]
    
    [(var? term1)
     (displayln (format "[DEBUG] Vinculando variável ~a = ~a" 
                      (extract-name term1) term2))
     (bind-variable term1 term2 env)]

    [(var? term2)
     (displayln (format "[DEBUG] Vinculando variável ~a = ~a" 
                      (extract-name term2) term1))
     (bind-variable term2 term1 env)]

    [(and (functor? term1) (functor? term2))
     (if (equal? (functor-name term1) (functor-name term2))
         (begin
           (displayln "[DEBUG] Functores com mesmo nome")
           (unify-args (functor-args term1) (functor-args term2) env))
         (begin
           (displayln "[DEBUG] Functores com nomes diferentes")
           #f))]

    [else 
     (displayln "[DEBUG] Tipos incompatíveis para unificação")
     #f]))

;; ---------------------------
;; Unificação de Argumentos (Corrigido para improper lists)
;; ---------------------------
(define (unify-args args1 args2 env)
  (displayln "[DEBUG] Unificando argumentos...")
  (cond
    [(and (null? args1) (null? args2)) 
     (displayln "[DEBUG] Todos argumentos processados")
     env]
    [(and (pair? args1) (pair? args2))
     (let ([new-env (unify (car args1) (car args2) env)])
       (if new-env
           (begin
             (displayln "[DEBUG] Par de argumentos unificado")
             (unify-args (cdr args1) (cdr args2) new-env))
           (begin
             (displayln "[DEBUG] Falha na unificação de argumentos")
             #f)))]
    [(and (not (null? args1)) (not (null? args2)))
     (displayln "[DEBUG] Último par de argumentos")
     (unify args1 args2 env)]
    [else 
     (displayln "[DEBUG] Número de argumentos incompatível")
     #f]))

;; ---------------------------
;; Função para associar variável (Modificado com mais logs)
;; ---------------------------
(define (bind-variable var value env)
  (if (assoc var env)
      env
      (begin
        (displayln (format "[DEBUG] Ambiente atualizado: ~a = ~a" 
                           (extract-name var) 
                           value))
        (cons (cons var value) env))))

;; ---------------------------
;; Predicados Auxiliares (Corrigido)
;; ---------------------------
(require (prefix-in ast: dcc019/exercise/logic/ast))

(define (var? x) (ast:var? x))
(define (functor? x) (ast:functor? x))
(define (clause-head c) (ast:clause-head c))
(define (clause-body c) (ast:clause-body c))

(define (functor-name f) (ast:functor-name f))
(define (functor-args f) (ast:functor-args f))