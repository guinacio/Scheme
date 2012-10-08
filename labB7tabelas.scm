#lang scheme
(require r5rs)
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;     Linguagem: R5RS.
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

;;; Tabelas 


;; Lista de associações (lista de pares)

; Retorna o primeiro par cujo primeiro elemento corresponde à chave
;(define (assoc chave listaa)
;  (cond ((null? listaa) #f)
;        ((equal? chave (caar listaa)) (car listaa))
;        (else (assoc chave (cdr listaa)))))


;; Dado abstrato: Tabela Unidimensional

; Construtor
(define (faz-tabela1)
  (list '*tabela1*))

; Seletor
(define (busca-tabela1 chave tabela)
  (let ((registro (assoc chave (cdr tabela))))
    (if registro
        (cdr registro)
        #f)))

; Mutador
(define (insere-tabela1! chave valor tabela)
  (begin
    (let ((registro (assoc chave (cdr tabela))))
      (if registro
          (set-cdr! registro valor)
          (set-cdr! tabela
                    (cons (cons chave valor) (cdr tabela)))))
    'ok))

; Exemplo
(define t1 (faz-tabela1))
t1
(insere-tabela1! 'c 3 t1)
(insere-tabela1! 'b 2 t1)
(insere-tabela1! 'a 1 t1)
t1
(busca-tabela1  'a t1)
(busca-tabela1  'd t1)


;; SUA TAREFA:
;;
;; 1. Implemente um procedimento mutador (remove-tabela1! chave tabela)
;;    que remova da tabela unidimensional o registro associado à chave.
(define (assoc1 chave listaa)
  (cond ((null? (cdr listaa)) #f)
        ((equal? chave (caadr listaa)) listaa)
        (else (assoc1 chave (cdr listaa)))))


(define (remove-tabela1! chave tabela)
   (let ((registro (assoc1 chave tabela)))
     (if registro
         (set-cdr! registro (cddr registro))
         (display "Chave não encontrada:  "))))
(remove-tabela1! 'a t1)
t1
;;
;; 2. Crie uma representação para tabela Bidimensional definindo o construtor, 
;;    o seletor e o mutador abaixo:

; Construtor
(define (faz-tabela2)
  (list '*tabela2*))

; Seletor
(define (busca-tabela2 chave1 chave2 tabela)
  null)
; Mutador
(define (insere-tabela2! chave1 chave2 valor tabela)
  "Completar aqui")

; Exemplo
(define t2 (faz-tabela2))
t2
(insere-tabela2! 'letras 'b 98 t2)
(insere-tabela2! 'operadores '* 42 t2)
(insere-tabela2! 'letras 'a 97 t2)
(insere-tabela2! 'operadores '- 45 t2)
(insere-tabela2! 'operadores '+ 43 t2)
t2
(busca-tabela2  'letras 'a t2)
(busca-tabela2  'operadores 'b t2)
(busca-tabela2  'operadores '- t2)

;;
;; 3. Defina um procedimento (imprime-tabela2 <tabela>) que apresente
;;    os dados da tabela de acordo com o exemplo abaixo.   
;;    (imprime-tabela2 t2)
;;    operadores:
;;            +:  43
;;            -:  45
;;            *:  42
;;    letras:
;;            a:  97
;;            b:  98
;;
;; 4. Os procedimentos busca-tabela2 e insere-tabela2! definidos acima recebem   
;;    uma tabela como argumento. Isso permite acessar mais que uma tabela nos   
;;    programas. Outra forma de fazer isso é gerar procedimentos    
;;    busca-tabela2 e insere-tabela2! separados para cada tabela, o que pode   
;;    ser realizado representado-se a tabela como um procedimento que mantém uma   
;;    tabela interna como parte do seu estado local. Ao enviar a mensagem   
;;    apropriada, essa tabela-objeto fornece o procedimento que permite operar   
;;    sobre sua tabela interna. Assim, complete a definição do procedimento   
;;    (faz-tabela-obj) abaixo, apresentando implementações para operações sobre   
;;    tabela nesta representação:
(define (faz-tabela-obj)
  ; tabela interna
  (define tabela-interna "...")
  ; seletor
  (define (busca-tabela2 chave1 chave2) "...")
  ; construtor
  (define (insere-tabela2! chave1 chave2 valor) "...")
  ; passagem de mensagens
  (define (despachar msg) "...")
  despachar)
;;
;; 5. Uma Tabela-Hash é uma tabEla unidimensional que permite representar um    
;;    conjunto de registros na estrutura de um vetor. A vantagem de vetores sobre
;;    listas encadeadas é que a partir do índice e do endereço inicial de um vetor, 
;;    pode-se acessar diretamente qualquer registro. A desvantagem é que o tamanho 
;;    do vetor (e a memória alocada) precisa ser fixado no momento de criação da 
;;    tabela, mesmo que boa parte desse vetor permaneça inutilizada. Para associar as 
;;    chaves aos índices do vetor, define-se uma função de hash H(k) = Ord(k) mod N, 
;;    onde Ord(k) é uma função que traduz cada chave k num número próprio. Como 
;;    normalmente o número de chaves existentes é muito maior que N, é possivel 
;;    (embora pouco esperado) que duas chaves k1 e k2 sejam associadas ao mesmo 
;;    índice do vetor, H(k1) = H(k2). Uma forma de resolver esse problema chamado de 
;;    colisão é definir cada elemento do vetor como uma lista encadeada e inserir 
;;    todos os elementos de mesmo índice na respectiva lisa. Crie uma representação 
;;    para tabela-hash definindo o construtor (faz-tabela-hash N), o seletor 
;;    (busca-tabela-hash chave) e os mutadores  (insere-tabela-hash! chave valor) 
;;    (remove-tabela-hash! chave). 
;;    Dicas: O procedimento (make-vector N null) cria um vetor com N listas vazias. 
;;    Os procedimentos (vector-ref V i) e (vector-set! V i) retornam e mudam 
;;    respecticamente o elemento i do vetor V. Como a função (hash chave) depende do 
;;    valor de N de cada tabela, é melhor representar a tabela-hash numa estrutura de
;;    objetos e definir a função hash internamente, conforme o modelo abaixo:
(define (faz-tabela-hash n)
  ; tabela e função internas
  (define tabela-interna "...")
  (define (hash chave) "...")
  ; seletor
  (define (busca-tabela-hash chave) "...")
  (define (insere-tabela-hash! chave valor)  "...")
  ; mutadores
  (define (remove-tabela-hash! chave)  "...")
  ; passagem de mensagens
  (define (despachar msg) "...")
  despachar)
