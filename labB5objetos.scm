#lang scheme
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;     Linguagem: Essentials of Programming Languages (2nd ed.).
;;;
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102
;;;
;;;   Programador(es): Guilherme Silva Inácio
;;;   E-mail: 

;;; Exemplo de Objeto: Conta bancária

;;; I) Operador de Atribuição

(define saldo 100)

(define (sacar qtd)
  (if (>= saldo qtd)
      (begin (set! saldo (- saldo qtd)) ; atribuição
             saldo)
      "Sem fundos"))

;; Forma Especial: (set! <nome> <novo-valor>)
;;   Muda o valor associado ao símbolo <nome> para o resultado da 
;;   expressão <novo-valor>.

(display "sacar:\n")

(sacar 25)
; 75
(sacar 25)
; 50
(sacar 60)
; "Sem fundos"
(set! saldo 2000000)
(sacar 1000000)
; 1000000

;; Observa-se que diferentemente dos procedimentos vistos até agora,
;; o procedimento "sacar" não corresponde a uma função, visto que duas 
;; chamadas com o mesmo argumento podem retornar valores distintos.
;; Isso ocorre porque o resultado do procedimento "sacar" depende do 
;; ESTADO da variável global "saldo", o qual pode MUDAR toda vez que o 
;; procedimento for executado.


;;; II) Variável de estado local

;; Encapsulamento da variável saldo-interno com valor inicial 100
(define sacar-novo
  (let ((saldo-interno 100)) 
    (lambda (qtd)    
      (if (>= saldo-interno qtd)
          (begin (set! saldo-interno (- saldo-interno qtd)) 
                 saldo-interno)
          "Sem fundos"))))

(display "\nsacar-novo:\n")
(sacar-novo 25)
; 75
(sacar-novo 25)
; 50
(sacar-novo 60)
; "Sem fundos"

;(set! saldo-interno 2000000)
; set!: cannot set undefined identifier: saldo-interno
(sacar-novo 1000000)
; "Sem fundos"

;; Encapsulamento da variável saldo com valor inicial dado pelo argumento
(define (faz-sacar saldo-interno)
  (lambda (qtd)
    (if (>= saldo-interno qtd)
        (begin (set! saldo-interno (- saldo-interno qtd)) 
               saldo-interno)
        "Sem fundos")))

(define S1 (faz-sacar 100))
(define S2 (faz-sacar 100)) ; S1 e S2 são dois objetos (instâncias) semelhantes, mas distintos
(define S3 S2) ; S2 e S3 são dois símbolos associados ao mesmo objeto

(display "\nfaz-sacar:\n")
(S1 50)
; 50
(S2 70)
; 30
(S2 40)
; "Sem fundos"
(S3 30)
; 0
(S1 40)
; 10

;(set! saldo-interno 2000000)
; set!: cannot set undefined identifier: saldo-interno
(S1 1000000)
; "Sem fundos"

;; Observa-se que S1 e S2 são procedimentos, cada qual com uma variável de 
;; estado local própria que pode mudar com o uso do respectivo procedimento.


;;; III) Passagem de Mensagem

(define (faz-conta saldo)
  (define (sacar qtd)
    (if (>= saldo qtd)
        (begin (set! saldo (- saldo qtd)) 
               saldo)
        "Sem fundos"))
  (define (depositar qtd)
    (begin (set! saldo (+ saldo qtd)) 
           saldo))
  (define (despachar msg)
    (cond ((eq? msg 'sacar) sacar)
          ((eq? msg 'depositar) depositar)
          (else (error "Pedido desconhecido -- FAZ-CONTA"
                       msg))))
  despachar)

(define C1 (faz-conta 100))

(display "\nfaz-conta:\n")
((C1 'sacar) 50)
; 50
((C1 'sacar) 60)
; "Sem fundos"
((C1 'depositar) 40)
; 90
((C1 'sacar) 60)
; 30


;; SUA TAREFA:
;;
;; 1. Uma das formas mais usadas para gerar um número pseudo-aleatório é o Método 
;;    Linear Congruente. Através deste método, os números são calculados por uma 
;;    seqüência de números naturais de período m, definida pela equação: 
;;
;;       X(i+1) = (X(i)*a + c) mod m,   para i>=0

(define c 1)
(define a 22695477)
(define m 4294967291)

(define (numero-aleatorio n)
  (let ((X (current-seconds)))
    (if (< n m)
          (begin (set! X (remainder (+ (* X a) c) m))
                 X)
          "N incompatível")))

(numero-aleatorio 88)
          
;;
;;    O valor inicial X(0) é chamado de semente (seed), a é o multiplicador, c o
;;    incremento e m é o módulo. O valor de X(0) pode ser escolhido aleatoriamente,
;;    mas as constantes a, c e m devem ser escolhidas adequadamente de modo que:
;;       i) c e m sejam primos entre si.
;;       ii) a - 1 seja um múltiplo de p, para todo p primo divisor de m
;;       iii) a - 1 seja um múltiplo de 4, se m for um múltiplo de 4.
;;    Implemente um procedimento (numero-aleatorio n) que a cada chamada retorne um  
;;    novo número aleatório entre 0 e n-1, com n < m. Para obter um valor entre 0 e  
;;    n-1 a partir de um número X(i) obtido pelo método acima, basta calcular X(i) mod n.  
;;    Esse procedimento deve ter uma variável de estado local com o valor atual da  
;;    seqüência X(i), iniciada com o resultado de (current-seconds). Pode-se usar as  
;;    seguintes constantes: a = 22695477,  c = 1 e m = 4294967291.
;;
;;    http://www.feferraz.net/files/lista/random_numbers.pdf
;;    http://en.wikipedia.org/wiki/Linear_congruential_generator
;;    http://random.mat.sbg.ac.at/
;;
;; 2. Modifique o procedimento "faz-conta" de modo que ele crie contas protegidas 
;;    por senhas. Para isso, o "faz-conta" deve receber um símbolo para senha como 
;;    argumento adicional, como em (define C1 (faz-conta 100 'senha-secreta)). O 
;;    objeto conta resultante deve processar um pedido somente se for acompanhado 
;;    da senha fornecida na criação da conta. Do contrário, deve retornar uma 
;;    advertência. Por exemplo: 
;;     ((C1 'senha-secreta 'sacar) 40) -> 60 
;;     ((C1 'senha-errada 'depositar) 50) -> "Senha incorreta"
;;
;; 3. Modifique o procedimento "faz-conta" da tarefa anterior, adicionando uma 
;;    outra variável de estado local de forma que, se uma conta for acessada mais 
;;    de sete vezes consecutivas com senhas incorretas, ele ative o procedimento 
;;    "chamar-a-policia".
;;
;; 4. Resolva as Tarefas 2 e 3 acima usando a linguagem C++.
  

