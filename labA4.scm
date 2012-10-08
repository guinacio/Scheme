#lang scheme
(require mzlib/trace)
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;   Linguagem: Module
;;;  
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102 Fundamentos da Estrutura da Informação
;;;
;;;   Programador(es):
;;;
;;;   Tel.: 0xx48 3721 7791
;;;   E-mail: max@das.ufsc.br
;;;
;;;   Laboratório A4: Números Primos
;;;

;;; INTRODUÇÃO

;; Função auxiliar (reuso!)
(define (quadrado x)(* x x))

;; Primo?
(define (menor-divisor n)
  (procura-div n 2))

(define (procura-div n div)
  (cond ((> (quadrado div) n) n)
        ((divide? n div) div)
        (else (procura-div n (+ div 1)))))

(define (divide? a b)
  (= (remainder a b) 0))

(define (primo? n)
  (= n (menor-divisor n)))

;(time (primo? 659804242921))

;; SUA TAREFA:
;;
;; 1.  O processo gerado pela função "primo?" é iterativo ou recursivo? Qual a  
;;    ordem de crescimento em tempo e em espaço? Testar a função "primo?" usando  
;;    "trace" e "time".
;;
(display "\n\n--------------- Tarefa 1 -----------------\n\n")
(trace procura-div)
(primo? 1021)
(untrace procura-div)
(begin (display "(primo? 659804242921)\n")
       (time (primo? 659804242921))
       (display "(primo? 199999999999)\n")
       (time (primo? 1999999999999))
       (display "\n"))

(display "O processo gerado é ITERATIVO com O(raiz(n)) em tempo e O(1) em espaço.\n")

;; 2. Criar uma função "primo2?", análoga a "primo?", mas que não teste múltiplos 
;;    de 2 e 3. Para torná-la mais eficiente que "primo?", "primo2?" deve testar apenas 
;;    se n é divisível por 2,3,5,7,11,13,17,(17+2),(19+4),(23+2),(25+4),...,raiz(n).
;;    Compare experimentalmente o desempenho de "primo2?" com "primo?".
;;
(display "\n\n--------------- Tarefa 2 -----------------\n\n")

(define (menor-divisor2 n)
  (cond ((divide? n 2) 2)
        ((divide? n 3) 3)
        (else (procura-div2 n 5 2))))

(define (procura-div2 n div k)
  (cond ((> (quadrado div) n) n) ; somente testa números menores do que raiz de n
        ((divide? n div) div)   
        (else (procura-div2 n (+ div k) (- 6 k))))) ; pula 2 e 4 números alternadamente de forma a não testar os pares nem os múltiplos de 3

(define (primo2? n)
  (= n (menor-divisor2 n)))

(trace procura-div2)
(primo2? 1021)
(untrace procura-div2)
(begin (display "(primo2? 659804242921)\n")
       (time (primo2? 659804242921))
       (display "(primo2? 199999999999)\n")
       (time (primo2? 1999999999999))
       (display "\n"))

;; 3. Segundo o "Pequeno Teorema de Fermat", para qualquer número primo n e qualquer 
;;    natural a tal que 0 < a < n, é sempre verdade que a^n mod n = a. Use as  
;;    primitivas "expt", "remainder" e "random" para programar uma função  
;;    "fermat-teste", de maneira que "log-primo?" possa funcionar. 
;;
;;    OBS: x mod y é o resto da divisão de x por y
(display "\n\n--------------- Tarefa 3 -----------------\n\n")

(define (log-primo? n vezes)
  (cond ((= vezes 0) #t)
        ((fermat-teste n) (log-primo? n (- vezes 1)))
        (else #f)))

(define (fermat-teste n)
  (testa (+ 1 (random n)) n))
(define (testa a n)
  (= (remainder (expt a n) n) a))

(trace testa)
(log-primo? 1021 10)
(untrace testa)
(begin (display "(log-primo? 58901 20)\n")
       (time (log-primo? 58901 20))
       (display "(log-primo? 19999 20)\n")
       (time (log-primo? 19999 20))
       (display "\n"))

;; 4. Para poder aplicar o teste de Fermat a números grandes a e n, deve-se obter o valor
;;    de a^n mod n sem calcular o valor de a^n. A definição recursiva abaixo permite 
;;    calcular eficientemente b^e mod m. 
;;
;;    b^e mod m  =   1,                         se e = 0;
;;                   ((b^(e/2) mod m)^2 mod m,  se e for par;
;;                   (b*(b^(e-1) mod m) mod m,  se e for ímpar.
;;
;;    Usando essa definição, implemente um procedimento recursivo (exp-mod b e m) e 
;;    utilize-o para implementar a função (fermat-teste2 n) de modo que o procedimento 
;;    (log-primo2? n vezes) possa testar rapidamente se um número grande é primo.

(display "\n\n--------------- Tarefa 4 -----------------\n\n")

(define (log-primo2? n vezes)
  (cond ((= vezes 0) #t)
        ((fermat-teste2 n) (log-primo2? n (- vezes 1)))
        (else #f)))

(define (fermat-teste2 n)
  (testa2 (+ 1 (random (remainder n 4294967087))) n))
(define (testa2 a n)
  (= (expmod a n n) a))

(define (expmod b n m)
  (cond ((= n 0) 1)
        ((even? n) (remainder (quadrado (expmod b (/ n 2) m)) 
                              m))
        (else (remainder (* b (expmod b (- n 1) m))
                         m))))

(trace testa2)
(log-primo2? 1021 10)
(untrace testa2)
(begin (display "(log-primo2? 659804242921 100)\n")
       (time (log-primo2? 659804242921 100))
       (display "(log-primo2? 199999999999 100)\n")
       (time (log-primo2? 199999999999 100))
       (display "(log-primo2? 282755483533707287054752184321121345766861480697448703443857012153264407439766013042402571 100)\n")
       (time (log-primo2? 282755483533707287054752184321121345766861480697448703443857012153264407439766013042402571 100))
       (display "(log-primo2? 199999999999999999999990000000000000000000000000000000000000000000000000000000000000002571 100)\n")
       (time (log-primo2? 199999999999999999999990000000000000000000000000000000000000000000000000000000000000002571 100))
       (display "\n"))

;; 5. Faça um procedimento (vinte-primos n) que mostre os menores 20
;;    números primos maiores do que n. 
;;
(display "\n\n--------------- Tarefa 5 -----------------\n\n")

(define (procura-primo n k kmax)
  (cond ((> k kmax) (display "fim\n")) ; somente testa números menores do que raiz de n
        ((log-primo2? n 100) (begin (display n)
                                    (newline)
                                    (procura-primo (+ n 1) (+ k 1) kmax)))  
        (else (procura-primo (+ n 1) k kmax))))

(define (vinte-primos n)
  (procura-primo n 1 20))

(vinte-primos 100000000)

;; 6. Existem alguns números que enganam o Teste de Fermat, isto é, 
;;    números n que não são primos e ainda "a^n mod n = a" para todos 
;;    os inteiros positivos a < n. Esses números, chamados de Números 
;;    de Carmichael, são extremamente raros. Os cinco primeiros são 561, 
;;    1105, 1729, 2465 e 2821. Faça um procedimento que calcule quantos 
;;    Números de Carmichael existem entre 1 e 100.000.
;;
(display "\n\n--------------- Tarefa 6 -----------------\n\n")

(define (fermat-teste-completo n)
  (define (tenta a)
    (cond [(>= a n) #t]
          [(= (expmod a n n) a) (tenta (+ 1 a))]
          [else #f]))
  (tenta 1))

(define (carmichael? n)
  (and (log-primo2? n 10) ; faz um pré-teste
       (not (primo2? n))
       (fermat-teste-completo n) ; só para confirmar
       )
  )

(define (procura-carmichael n k nmax)
  (cond ((> n nmax) k) ; somente testa números menores do que raiz de n
        ((carmichael? n) (begin (display (+ k 1)) (display " ") (display n)
                                (newline)
                                (procura-carmichael (+ n 1) (+ k 1) nmax)))  
        (else (procura-carmichael (+ n 1) k nmax))))

(define (quantos-carmichael n)
  (procura-carmichael 2 0 n))

(quantos-carmichael 100000) 
;;
;;
;; Wikipédia: http://en.wikipedia.org/wiki/Prime_number
;; Listas de números primos: http://primes.utm.edu/lists/small/small.html
;;                           http://primes.utm.edu/cgi-bin/links/jump.cgi?ID=164
