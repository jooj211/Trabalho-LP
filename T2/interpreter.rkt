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
    [(ast:unknow-var? term)
     (let ([new-var (ast:var (gensym '_))])
       (values new-var env))]
    [(ast:atom? term) (values term env)]
    [(ast:functor? term)
     (let-values ([(new-args new-env) (rename-term-list (ast:functor-args term) env)])
       (values (ast:functor (ast:functor-name term) new-args) new-env))]
    [else (values term env)]))

;; ---------------------------
;; Renomeação de Termos (Corrigido com Fechamento Correto de Parênteses)
;; ---------------------------
(define (rename-term-list terms env)
  (cond
    [(null? terms) 
     (values '() env)]
    
    [(pair? terms) ; Se for um par, processa car e cdr normalmente
     (call-with-values
      (lambda () (rename-term (car terms) env))
      (lambda (new-term env1)
        (call-with-values
         (lambda () (rename-term-list (cdr terms) env1))
         (lambda (new-terms env2)
           (values (cons new-term new-terms) env2)))))] ; <<< Parêntese faltante adicionado aqui
    
    [else ; Se for um átomo (improper list)
     (call-with-values
      (lambda () (rename-term terms env))
      (lambda (new-term env1)
        (values (list new-term) env1)))]))

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
;; ---------------------------
;; Busca por Fatos e Regras
;; ---------------------------
;; ---------------------------
;; Busca por Fatos e Regras (Versão Corrigida)
;; ---------------------------
(define (search-rules query env rules)
  (if (null? rules)
      '()
      (let* ([rule (car rules)]
             [renamed-rule (rename-clause rule)]
             [unif (unify query (clause-head renamed-rule) env)])
        (append
         (if unif
             (resolve-query-body (clause-body renamed-rule) unif)
             '())
         (search-rules query env (cdr rules))))))


;; ---------------------------
;; Unificação de Termos (Com Debugs Aprimorados)
;; ---------------------------

(define (walk term env)
  (if (and (ast:var? term) (assoc term env))
      (walk (cdr (assoc term env)) env)
      term))

(define (unify term1 term2 env)
  (displayln (format "\n[DEBUG] Iniciando unificação de ~a e ~a" term1 term2))
  (displayln (format "[DEBUG] Ambiente inicial:"))
  (display-env env)
  
  (let ([t1 (walk term1 env)]
        [t2 (walk term2 env)])
    (displayln (format "[DEBUG] Após walk: ~a → ~a | ~a → ~a" term1 t1 term2 t2))
    
    (cond
      [(equal? t1 t2)
       (displayln "[DEBUG] Termos idênticos!")
       env]
      
      [(ast:var? t1)
       (displayln (format "[DEBUG] Vinculando variável ~a = ~a" t1 t2))
       (bind-variable t1 t2 env)]
      
      [(ast:var? t2)
       (displayln (format "[DEBUG] Vinculando variável ~a = ~a" t2 t1))
       (bind-variable t2 t1 env)]
      
      [(and (ast:functor? t1) (ast:functor? t2))
       (if (equal? (ast:functor-name t1) (ast:functor-name t2))
           (begin
             (displayln "[DEBUG] Functores com mesmo nome. Unificando argumentos...")
             (unify-args (ast:functor-args t1) (ast:functor-args t2) env))
           (begin
             (displayln "[DEBUG] Functores diferentes!")
             #f))]
      
      [else
       (displayln "[DEBUG] Tipos incompatíveis para unificação!")
       #f])))

;; ---------------------------
;; Unificação de Argumentos (Tratamento de Improper Lists)
;; ---------------------------
(define (unify-args args1 args2 env)
  (displayln (format "[DEBUG] Unificando argumentos: ~a vs ~a" args1 args2))
  (let ([lst1 (ensure-proper-list args1)]
        [lst2 (ensure-proper-list args2)])
    (displayln (format "[DEBUG] Argumentos convertidos: ~a vs ~a" lst1 lst2))
    (cond
      [(and (null? lst1) (null? lst2))
       (displayln "[DEBUG] Argumentos unificados com sucesso!")
       env]
      
      [(and (pair? lst1) (pair? lst2))
       (displayln "[DEBUG] Unificando par de argumentos...")
       (let ([new-env (unify (car lst1) (car lst2) env)])
         (if new-env
             (begin
               (displayln "[DEBUG] Par unificado. Continuando...")
               (unify-args (cdr lst1) (cdr lst2) new-env))
             (begin
               (displayln "[DEBUG] Falha na unificação do par!")
               #f)))]
      
      [else
       (displayln "[DEBUG] Número de argumentos incompatível!")
       #f])))

;; ---------------------------
;; Conversão de Improper Lists para Proper Lists (Nova Função)
;; ---------------------------
(define (ensure-proper-list args)
  (cond
    [(null? args) 
     (begin
       (displayln "[DEBUG] Lista vazia. Nada a converter.")
       '())]
    [(pair? args)
     (begin
       (displayln (format "[DEBUG] Convertendo improper list: ~a" args))
       (cons (car args) (ensure-proper-list (cdr args))))]
    [else
     (begin
       (displayln (format "[DEBUG] Elemento final da improper list: ~a" args))
       (list args))]))

;; ---------------------------
;; Funções Auxiliares para Desigualdades (Com Debugs)
;; ---------------------------
(define (check-diff env term1 term2)
  (displayln (format "[DEBUG] Verificando desigualdade: ~a ≠ ~a" term1 term2))
  (let* ([val1 (walk term1 env)]
         [val2 (walk term2 env)])
    (displayln (format "[DEBUG] Valores após substituição completa: ~a ≠ ~a" val1 val2))
    (not (terms-equal? val1 val2))))

(define (terms-equal? t1 t2)
  (displayln (format "[DEBUG] Comparando estruturalmente: ~a vs ~a" t1 t2))
  (let ([t1 (walk t1 '())] ; Força caminhada completa sem ambiente (valores finais)
        [t2 (walk t2 '())])
    (cond
      [(and (ast:var? t1) (ast:var? t2)) (eq? t1 t2)]
      [(and (ast:functor? t1) (ast:functor? t2))
       (and (equal? (ast:functor-name t1) (ast:functor-name t2))
            (andmap terms-equal? 
                    (ast:functor-args t1) 
                    (ast:functor-args t2)))]
      [else (equal? t1 t2)])))
;; ---------------------------
;; Associação de Variáveis no Ambiente
;; ---------------------------
(define (bind-variable var value env)
  (let ([val (walk value env)])
    (if (occurs-check var val env)
        #f
        (cons (cons var val) env))))

(define (occurs-check var term env)
  (let ([term (walk term env)])
    (cond
      [(eq? var term) #t]
      [(ast:functor? term)
       (ormap (lambda (arg) (occurs-check var arg env)) (ast:functor-args term))]
      [else #f])))

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
