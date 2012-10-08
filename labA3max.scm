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
;;;   Programador: Max Hering de Queiroz
;;;
;;;   Tel.: 0xx48 3721 7791
;;;   E-mail: max@das.ufsc.br

;;;
;;;   Laboratório A3: Ordem de Crescimento
;;;

;;; INTRODUÇÃO

;; Função auxiliar (reuso!)
(define (quadrado x)(* x x))



;;==================== Exponencial ============================

;; a Exponencial A
(define (exp-a b n)
  (if (= n 0)
      1
      (* b (exp-a b (- n 1)))))

;; b Exponencial B
(define (exp-b b n)
  (exp-ir b n 1))
(define (exp-ir b counter product)
  (if (= counter 0)
      product
      (exp-ir b
              (- counter 1)
              (* b product)))) 


;; c Exponencial C
(define (exp-c b n)
  (cond ((= n 0) 1)
        ((even? n) (quadrado (exp-c b (/ n 2))))
        (else (* b (exp-c b (- n 1))))))


;; SUA TAREFA:
;;
;; 1. Analise os três procedimentos acima para cálculo de exponencial com expoentes inteiros, indicando se os processos gerados por eles são recursivos ou iterativos e também qual a sua ordem de crescimento em tempo e em espaço. Verifique sua análise, usando os procedimentos TRACE e TIME.
;;
;;    (time (<função> <parâmetros>))
;;
(begin (display "(exp-a b n): O processo é recursivo e requer O(n) passos e O(n) espaço.\n")
       (display "(exp-a 2 100000)\n")
       (void (time (exp-a 2 100000)))
       (display "\n"))
;  cpu time: 8191 real time: 8221 gc time: 6199

(begin (display "(exp-a b n): O processo é iterativo e requer O(n) passos e O(1) espaço.\n")
       (display "(exp-b 2 100000)\n")
       (void (time (exp-b 2 100000)))
       (display "\n"))
;  cpu time: 11276 real time: 11346 gc time: 9745

(begin (display "(exp-a b n): O processo é recursivo e requer O(log(n)) passos e O(log(n)) espaço.\n")
       (display "(exp-c 2 100000)\n")
       (void (time (exp-c 2 100000)))
       (display "\n"))
;  cpu time: 0 real time: 0 gc time: 0


;; 2. Para ângulos pequenos (menores do que 0.1 radianos) a função
;;    seno pode ser aproximada pelo próprio ângulo:
;;
;;    seno(x) ~ x
;;
;;    Para ângulos maiores do que 0.1 radianos, pode-se diminuir o
;;    ângulo usando a fórmula:
;;
;;    seno(x) = 3 seno(x/3) - 4 (seno(x/3))^3
;;            = 3y - 4y^3, com y = seno(x/3)
;;
;;    escreva um procedimento que calcule o seno utilizando estes
;;    fatos (o parâmetro deve ser em radianos).
;;

(define (seno x)
  (define (aux s)
    (- (* 3 s) (* 4 s s s)))
  (if (< x 0.0001)
      x
      (aux (seno (/ x 3.0)))))

(seno (/ pi 2))

;; 3. Qual a ordem de crescimento em espaço e tempo (em relação ao
;;    tamanho do ângulo de entrada) da função seno implementada na 
;;    Questão 2?
;;

; Como o processo é recursivo linear e a cada passo divide-se o 
; parâmetro por três, temos O(log_3(x)) = O(log(x)/log(3)) = O(log(x)) 
; tanto em espaço quanto em tempo.

;; 4. A função iterativa que calcula os números de Fibonacci, pode 
;;    ser vista como a aplicação da transformada:
;;
;;    T: a <- a + b, 
;;       b <- a
;;
;;    Assim, o enésimo número corresponde à aplicação de T^n.
;;    Considere esta traformação como um caso particular, para
;;    p=0 e q=1, da seguinte tansformação:
;;
;;    Tpq: a <- bq + aq + ap, 
;;         b <- bp + aq
;;
;;    A aplicação desta transformada por duas vezes é equivalente
;;    à aplicação de uma transformada Tp'q' com a mesma forma de Tpq.
;;    Calcule os valores de p' e q' em termos de p e q, obtendo uma
;;    maneira explícita de elevar ao quadrado a transformada Tpq.
;;    Complete então a função "fib-iter", que calcula os números de
;;    Fibonacci em tempo logarítmico, para obter o valor de (fib 200000)
;;    em menos e 1 segundo.

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p)) ; calcula p'
                   (+ (* q q) (* 2 p q)) ; calcula q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


(fib 200000)