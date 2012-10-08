#lang scheme
(require mzlib/trace)
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;; Linguagem: Pretty Big
;;;
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102
;;;
;;;   Programador(es):
;;;   E-mail:



;; ==================== Dado abstrato árvore binária ==================== 

; Construtor
(define (faz-arv val esq dir)
  (list val esq dir))

(define (vazia) null)

; Seletores


(define (val arv)
  (car arv))

(define (esq arv)
  (cadr arv))

(define (dir arv)
  (caddr arv))

(define (vazia? arv) (null? arv))

;; ==================== Funções de transformação ==================== 

; Árvore para lista
(define (arv->lista a)
  (cond ((vazia? a) null)
        (else (append (arv->lista (esq a))
                      (cons (val a)
                            (arv->lista (dir a)))))))

; Árvore para lista
(define (arv->lista+ a)
  (define (copia arv lista)
    (cond ((vazia? arv) lista)
          (else (copia (esq arv)
                       (cons (val arv)
                             (copia (dir arv)
                                    lista))))))
  (copia a null))


;; ==================== Dado abstrato conjunto ==================== 

; Construtor
(define (insere x A)
  (cond ((vazia? A) (faz-arv x (vazia) (vazia)))
        ((= x (val A)) A)
        ((< x (val A)) (faz-arv (val A)
                                (insere x (esq A))
                                (dir A)))
        (else (faz-arv (val A)
                       (esq A)
                       (insere x (dir A))))))

(define (vazio) (vazia))

; Seletor
(define (pertence? x A)
  (cond ((vazia? A) #f)
        ((= x (val A)) #t)
        ((< x (val A)) (pertence? x (esq A)))
        (else (pertence? x (dir A)))))




;;  ==================== Exemplos ==================== 

(define a0 (insere 1 (insere 2 (insere 3 (insere 4 (insere 5 (vazia)))))))

(define a1 (insere 5 (insere 3 (insere 7 (insere 1 (insere 9 (vazia)))))))

(define a2 (insere 6 (insere 4 (insere 8 (insere 2 (insere 10 (vazia)))))))

; Cria conjunto a partir de n números aleatórios entre 0 e n-1
(define (conjunto-aleatorio n)
  (define (insere-outro cont A)
    (if (<= cont 0)
        A
        (insere-outro (- cont 1) (insere (random n) A))))
  (insere-outro n (vazio)))

(define G (conjunto-aleatorio 10000))
(pertence? 1000 G)
(pertence? 2000 G)
(pertence? 3000 G)
(pertence? 4000 G)
(pertence? 5000 G)


;; SUA TAREFA:
;;
;; 1. Implemente um procedimento (numel-arv a) que calcule o número de elementos 
;;    de uma árvore binária.
(define (numel-arv a)
  (cond 
    ((equal? a null) 0)
    (else (+ 1 (numel-arv (esq a)) (numel-arv (dir a))))))

(numel-arv a0)
;;
;; 2. Analisar o procedimento "arv->lista+" que dada uma árvore devolve uma
;;    lista ordenada, e explicar porque ele é mais eficiente do que
;;    "arv->lista".
(trace arv->lista)
(arv->lista a1)
(untrace arv->lista)
(trace arv->lista+)
(arv->lista+ a1)
(untrace arv->lista+)

(display "arv->lista+ é mais eficiente pois não abre todas as arvores simultaneamente para depois montar, nem utiliza o append que é menos eficiente que o cons; ela primeiro verifica se a arvore e o ramo são vazios, apos, abre a parte direita retorna o resultado e já monta este com o var, depois abre a da esquerda e monta a lista já ordenada\n")
;;
;; 3. Desenvolva um procedimento (ordena L) que receba uma lista 
;;    desordenada de números e retorne o conjunto dos números na forma 
;;    de uma lista ordenada. Sugestão: inserir todos os elementos numa
;;    árvore binária e usar o procedimento "arv->lista".
;;    Ex.: (ordena '(4 10 2 10 0 5 2)) -> (0 2 4 5 10)
;;
(define (ordena L)
  (arv->lista+ (aux1 L)))
(define (aux1 L)
  (cond
    ((equal? L null) (vazia))
    (else (insere (car L) (aux1 (cdr L))))))

(ordena '(4 10 2 10 0 5 2))
;; 4. Escrever um procedimento "lista-ord->arv" que dada uma lista 
;;    ordenada de números cria uma árvore balanceada com os elementos 
;;    dessa lista.
(define (lista-ord->arv L)
  (arv-aux (insere (meiolista L (contalista L)) null) (parte-esq L (meiolista L (contalista L)) null) (parte-dir L (meiolista L (contalista L)))))
    
(define (arv-aux novaarv esquerda direita)
  (cond
    ((equal? esquerda null) (arv-aux1 novaarv direita))
    (else (arv-aux (insere (car esquerda) novaarv) (cdr esquerda) direita))))

(define (arv-aux1 novaarv direita)
  (cond
     ((equal? direita null) novaarv)
     (else (arv-aux1 (insere (car direita) novaarv) (cdr direita)))))

(define (contalista L)
  (cond 
    ((equal? L null) 0)
    (else (+ 1 (contalista (cdr L))))))

(define (meiolista L n)
  (meiolista-aux L (floor (/ n 2))))
(define (meiolista-aux L cont)  
  (cond
    ((= cont 0) (car L))
    (else (meiolista-aux (cdr L) (- cont 1)))))

(define (parte-esq L n nova)
  (ordena (parte-esq-aux L n null)))
(define (parte-esq-aux L n nova)   
  (cond
    ((equal? (car L) n) nova) 
    (else (parte-esq-aux (cdr L) n (cons (car L) nova)))))

(define (parte-dir L n)
  (parte-dir-aux L n))
(define (parte-dir-aux L n)   
  (cond
    ((equal? (car L) n) (cdr L)) 
    (else (parte-dir-aux (cdr L) n))))

(define Cexerc5 (lista-ord->arv '(0 2 4 5 10)))
Cexerc5

;;
;; 5. Escrever procedimentos "inter" e "uniao", com ordem de crescimento O(n), para
;;    conjuntos representados na forma de árvores binárias. Sugestão: converter os 
;;    conjuntos em listas ordenadas, fazer a operação e então calcular a árvore.
;;

;; 6. Desafio: escrever um procedimento de impressão para o dado abstrato 
;;    "arvore" usando as primitivas:
;;    (display <objeto>) : imprime <objeto>
;;    (newline) : pula uma linha
;; 