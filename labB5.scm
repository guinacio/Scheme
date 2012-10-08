#lang scheme
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;     Linguagem: Pretty Big.
;;;
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102
;;;
;;;   Programador(es): Max Hering de Queiroz
;;;
;;;
;;;   Tel.: 0xx48 3721 7791
;;;   Fax: 0xx48 3721 9934
;;;   E-mail: max@das.ufsc.br

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

(define CA (faz-conta 100))
(define CB (faz-conta 0))
(display "\nfaz-conta:\n")
((CA 'sacar) 50)
; 50
((CA 'sacar) 60)
; "Sem fundos"
((CA 'depositar) 40)
; 90
((CA 'sacar) 60)
; 30


;; SUA TAREFA:
;;
;; 1. Uma das formas mais usadas para gerar um número pseudo-aleatório é o Método 
;;    Linear Congruente. Através deste método, os números são calculados por uma 
;;    seqüência de números naturais de período m, definida pela equação: 
;;
;;       X(i+1) = (X(i)*a + c) mod m,   para i>=0
;;
;;    O valor inicial X(0) é chamado de semente (seed), a é o multiplicador, c o
;;    incremento e m é o módulo. O valor de X(0) pode ser escolhido aleatoriamente,
;;    mas as constantes a, c e m devem ser escolhidas adequadamente de modo que:
;;       i) c e m sejam primos entre si.
;;       ii) a - 1 seja um múltiplo de p, para todo p primo divisor de m
;;       iii) a - 1 seja um múltiplo de 4, se m for um múltiplo de 4.
;;    Implemente um procedimento (numero-aleatorio n) que a cada chamada retorne um  
;;    novo número aleatório entre 0 e n-1. Para obter um valor entre 0 e n-1 a partir  
;;    de um número X(i) obtido pelo método acima, basta calcular X(i) mod n. Esse  
;;    procedimento deve ter uma variável de estado local com o valor atual da  
;;    seqüência X(i), iniciada com o resultado de (current-seconds). Pode-se usar as  
;;    seguintes constantes: a = 22695477,  c = 1 e m = 4294967291.
;;
;;    http://www.feferraz.net/files/lista/random_numbers.pdf
;;    http://en.wikipedia.org/wiki/Linear_congruential_generator
;;    http://random.mat.sbg.ac.at/
;;
(display "\nnumero-aleatorio:\n")
(define numero-aleatorio
  (let ((X (current-seconds))
        ;(m (- (expt 2 32) 5)) (a 22695477) (c 1))
        (m (- (expt 2 128) 159)) (a 243267374564284687042667403923350539132)(c 0))
    (lambda (n)
      (if (> n m) 
          (error "O máximo é" m)
          (begin 
            (set! X (remainder (+ (* a X) c) m)) 
            (remainder X n))))))

; Procedimento que gera m números aleatórios entre 0 e n-1 pela função f e
; retorna a lista com o total de ocorrências de cada número
(define (testador f n m)
  (define (zeros k)
    (build-list k (lambda (x) 0)))
  (define (contabiliza x lista)
    (if (= x 0)
        (cons (+ (car lista) 1) (cdr lista))
        (cons (car lista) (contabiliza (- x 1) (cdr lista)))))
  (define (testa cont lista)
    (if (< cont 1)
        lista
        (testa (- cont 1) (contabiliza (f n) lista))))
  (testa m (zeros n)))

(testador numero-aleatorio 2 10000)
(testador numero-aleatorio 3 10000)
(testador numero-aleatorio 4 10000)
(testador numero-aleatorio 5 10000)
(testador numero-aleatorio 6 10000)
(testador numero-aleatorio 7 10000)
(testador numero-aleatorio 8 10000)
(testador numero-aleatorio 9 10000)
(testador numero-aleatorio 10 10000)

; http://www.feferraz.net/files/lista/random_numbers.pdf
; http://en.wikipedia.org/wiki/Linear_congruential_generator
; http://random.mat.sbg.ac.at/

;; 2. Modifique o procedimento "faz-conta" de modo que ele crie contas protegidas 
;;    por senhas. Para isso, o "faz-conta" deve recebe um símbolo para senha como 
;;    argumento adicional, como em (define C1 (faz-conta 100 'senha-secreta)). O 
;;    objeto conta resultante deve processar um pedido somente se for acompanhado 
;;    da senha fornecida na criação da conta. Do contrário, deve retornar uma 
;;    advertência. Por exemplo: 
;;     ((C1 'senha-secreta 'sacar) 40) -> 60 
;;     ((C1 'senha-errada 'depositar) 50) -> "Senha incorreta"
;;

(define (faz-conta2 saldo senha)
  (define (sacar qtd)
    (if (>= saldo qtd)
        (begin (set! saldo (- saldo qtd)) 
               saldo)
        "Sem fundos"))
  (define (depositar qtd)
    (begin (set! saldo (+ saldo qtd)) 
           saldo))
  (define (incorreta qtd)
    "Senha incorreta")
  (define (despachar senha-fornecida msg)
    (cond ((not (eq? senha-fornecida senha)) incorreta)
          ((eq? msg 'sacar) sacar)
          ((eq? msg 'depositar) depositar)
          (else (error "Pedido desconhecido -- FAZ-CONTA"
                       msg))))
  despachar)

(display "\nfaz-conta (com senha):\n")
(define C2 (faz-conta2 100 'senha-secreta))
((C2 'senha-secreta 'sacar) 40)
((C2 'senha-errada 'depositar) 50)

;; 3. Modifique o procedimento "faz-conta" da tarefa anterior, adicionando uma 
;;    outra variável de estado local de forma que, se uma conta for acessada mais 
;;    de sete vezes consecutivas com senhas incorretas, ele ative o procedimento 
;;    "chamar-a-policia".

(define (faz-conta3 saldo senha)
  (define numero-de-incorretas 0)
  (define (sacar qtd)
    (if (>= saldo qtd)
        (begin (set! saldo (- saldo qtd)) 
               saldo)
        "Sem fundos"))
  (define (depositar qtd)
    (begin (set! saldo (+ saldo qtd)) 
           saldo))
  (define (incorreta qtd)
    "Senha incorreta")
  (define (chamar-a-policia qtd)
    "Corra, que a polícia vem aí!")
  (define (despachar senha-fornecida msg)
    (cond ((not (eq? senha-fornecida senha)) 
           (begin (set! numero-de-incorretas (+ numero-de-incorretas 1))
                  (if (>= numero-de-incorretas 7)
                      chamar-a-policia
                      incorreta)))
          (else (begin (set! numero-de-incorretas 0)
                       (cond ((eq? msg 'sacar) sacar)
                             ((eq? msg 'depositar) depositar)
                             (else (error "Pedido desconhecido -- FAZ-CONTA"
                                          msg)))))))
  despachar)

(display "\nfaz-conta (com senha protegida):\n")
(define C3 (faz-conta3 100 'senha-secreta))
((C3 'senha-secreta 'sacar) 40)
((C3 'senha-errada 'depositar) 50)
((C3 'outra-senha-errada 'depositar) 50)
((C3 'senha-secreta 'depositar) 50)
((C3 'senha-errada 'sacar) 1000)
((C3 'outra-senha-errada 'sacar) 1000)
((C3 'outra-senha-errada 'sacar) 1000)
((C3 'outra-senha-errada 'sacar) 1000)
((C3 'outra-senha-errada 'sacar) 1000)
((C3 'outra-senha-errada 'sacar) 1000)
((C3 'outra-senha-errada 'sacar) 1000)
((C3 'outra-senha-errada 'sacar) 1000)

;; 4. Resolva as Tarefas 2 e 3 acima usando a linguagem C++.
 