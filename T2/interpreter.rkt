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
             (values new-var (cons (cons term new-var) env)))))] ; <<< Fecha o let e o cond
    [(ast:atom? term) (values term env)] ; Átomos não precisam de renomeação
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
  (displayln (format "[DEBUG] Resolvendo corpo da regra: ~a" goals))
  (cond
    ;; Caso base: corpo vazio (improper list)
    [(null? goals) 
     (displayln "[DEBUG] Corpo vazio. Retornando ambiente.")
     (list env)]
    
    ;; Caso para desigualdade (neq)
    [(ast:neq? (if (pair? goals) (car goals) goals)) ; <--- Tratamento de improper list
     (displayln "[DEBUG] Processando condição de desigualdade...")
     (let* ([neq-expr (if (pair? goals) (car goals) goals)] ; <--- Safe access
            [term1 (ast:neq-lterm neq-expr)]
            [term2 (ast:neq-rterm neq-expr)]
            [diff-ok? (check-diff env term1 term2)])
       (displayln (format "[DEBUG] Verificando ~a ≠ ~a no ambiente:" term1 term2))
       (display-env env)
       (if diff-ok?
           (begin
             (displayln "[DEBUG] Desigualdade satisfeita!")
             (resolve-query-body (if (pair? goals) (cdr goals) '()) env)) ; <--- Safe cdr
           (begin
             (displayln "[DEBUG] Desigualdade falhou!")
             '())))]
    
    ;; Caso padrão para submetas
    [(pair? goals) ; <--- Garante que goals é um par antes de usar car/cdr
     (let ([first-goal (car goals)])
       (displayln (format "[DEBUG] Processando subgoal: ~a" first-goal))
       (let ([first-results (search-rules first-goal env knowledge-base)])
         (displayln (format "[DEBUG] Resultados parciais para ~a: ~a" first-goal first-results))
         (apply append 
                (map (lambda (env1)
                       (resolve-query-body (cdr goals) env1))
                     first-results))))]
    
    [else ; Improper list (último elemento não é par)
     (displayln "[DEBUG] Último elemento do corpo (improper list)")
     (resolve-query-body (list goals) env)]))

;; ---------------------------
;; Busca por Fatos e Regras
;; ---------------------------
;; ---------------------------
;; Busca por Fatos e Regras (Versão Corrigida)
;; ---------------------------
(define (search-rules query env rules)
  (displayln (format "[DEBUG] Buscando em ~a regra(s)" (length rules)))
  (if (null? rules)
      (begin
        (displayln "[DEBUG] Nenhuma regra restante!")
        '())
      (let* ([rule (car rules)]
             [renamed-rule (rename-clause rule)]) ; Renomeia variáveis da regra
        (displayln (format "[DEBUG] Testando regra: ~a" (clause-head renamed-rule)))
        (displayln (format "[DEBUG] Corpo da regra: ~a" (clause-body renamed-rule)))
        (let ([unif (unify query (clause-head renamed-rule) env)])
          (if unif
              (begin
                (displayln "[DEBUG] Unificação bem-sucedida!")
                (displayln "[DEBUG] Ambiente após unificação:")
                (display-env unif)
                (let ([body-results (resolve-query-body (clause-body renamed-rule) unif)])
                  (displayln (format "[DEBUG] Resultados do corpo: ~a" body-results))
                  (append body-results (search-rules query env (cdr rules))))) ; Fecha o let e o append
              (begin
                (displayln "[DEBUG] Unificação falhou. Próxima regra...")
                (search-rules query env (cdr rules))))))) ; Fecha o if, let e let*
)


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
  (let ([val1 (walk term1 env)]
        [val2 (walk term2 env)])
    (displayln (format "[DEBUG] Valores substituídos: ~a ≠ ~a" val1 val2))
    (not (terms-equal? val1 val2))))


(define (terms-equal? t1 t2)
  (displayln (format "[DEBUG] Comparando termos estruturalmente: ~a vs ~a" t1 t2))
  (cond
    [(and (ast:var? t1) (ast:var? t2)) 
     (eq? t1 t2)]
    [(and (functor? t1) (functor? t2))
     (and (equal? (functor-name t1) (functor-name t2))
          (terms-equal? (functor-args t1) (functor-args t2)))]
    [else 
     (equal? t1 t2)]))

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
