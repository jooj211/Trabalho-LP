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
  (set! knowledge-base prog))

;; ---------------------------
;; Função auxiliar: converte uma consulta (que pode ser um improper list) em uma lista própria de goals
;; ---------------------------
(define (goals->list q)
  (cond
    [(null? q) '()]
    [(pair? q) (cons (car q) (goals->list (cdr q)))]
    [else (list q)]))

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
          (display "False\n\n")
          #'(void))
        (begin
          (displayln "[DEBUG] Consulta bem-sucedida, exibindo soluções...")
          (display "True\n\n")
          (for-each (lambda (env)
                      (display-env env))
                    results)
          (newline) #'(void)))))

;; ---------------------------
;; Resolução de Consultas
;; ---------------------------
;; resolve-query: resolve uma única consulta (um goal)
(define (resolve-query query env)
  (displayln (format "[DEBUG] Resolvendo consulta: ~a" query))
  (if (null? knowledge-base)
      (begin (displayln "[DEBUG] Base de conhecimento vazia!") '())
      (search-rules query env knowledge-base)))

;; resolve-query-body: resolve uma sequência de subgoals (consulta composta)
(define (resolve-query-body goals env)
  (displayln (format "[DEBUG] Resolvendo consulta composta: ~a" goals))
  (if (null? goals)
      (list env)
      (let* ([first-goal (car goals)]
             [first-results (resolve-query first-goal env)])
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
             [unif (unify query head env)])
        (displayln (format "[DEBUG] Testando regra: ~a" head))
        (if unif
            (let ([result (if (null? body)
                              (list unif)
                              (resolve-query-body body unif))])
              (displayln "[DEBUG] Unificação bem-sucedida.")
              (append result (search-rules query env (cdr rules))))
            (search-rules query env (cdr rules))))))

;; ---------------------------
;; Unificação de Termos
;; ---------------------------
;; Função walk: obtém a substituição atual de um termo (para variáveis)
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
