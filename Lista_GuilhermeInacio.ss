#lang scheme
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
;;;   Programador(es): Guilherme Silva Inácio
;;;
;;;
;;;   Laboratório A1: Programação em Scheme
;;;

;;; INTRODUÇÃO

;; 1) Expressões Primitivas

;; i) Dados primitivos

#t  ; booleanos
486 ; números
#\a ; caracteres
'nome ; símbolo
"Scheme" ; texto (string)

;; ii) procedimentos primitivos

+   ; operadores
sin ; funções matemáticas
display ; procedimentos

;; 2) Meios de Combinação

(* 5 99)

(< 2.7 10)

(eq? 'nome "nome")

(and #t #f)

(* 25 4 12)

(+ (* 3 5) (- 10 6))

(+ (* 3
      (+ (* 2 4 6)
         (+ 3 5)))
   (+ (- 10 7)
      6))

(begin (display "\tDigite um valor:\n")
       ;      (display (read))
       (display "\nfim\n"))

;; 3) Meios de abstração

(define base 10) ; base <-- 10

(define altura 17.5) ; altura <-- 17.5

(define areaRetangulo (* base altura)) ; areaRetangulo <-- 175.0

areaRetangulo


;; 4) Abstração de procedimentos

; Função que eleva ao quadrado
(define (quadrado x) (* x x))

(quadrado 10)

; Função valor absoluto
(define (absoluto x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        (else x)))

(absoluto -1)
(absoluto 1)


;;; SUA TAREFA:
;;
;; 1. Escreva as expressões abaixo na notação pré-fixada de Scheme, e 
;;    execute-as:
;;
(display "\n--------Tarefa 1--------\n")
;;    a. (2 + 4) × 3
(display "A) ")
(* (+ 4 2) 3)

;;    b. 1+2+3+4+5+6+7+8+9
(display "B) ")
(+ 1 2 3 4 5 6 7 8 9)

;;    c. (raiz(2)+4) × (3-5) × (5/2)
(display "C) ")
(* (+ (sqrt 2) 4) (- 3 5) (/ 5 2))

;;    d. sen(pi/2) × (8+(4×3))
(display "D) ")
(* (sin (/ pi 2)) (+ 8 (* 4 3)))

;;    e. log(2^10) + cos(0)
(display "E) ")
(+ (log (expt 2 10)) (cos 0))

;;    f. ((V ou F) e (F e V))
(display "F) ")
(and (or #t #f) (and #f #t))

;;
;; 2. Associe o nome "perimetro" ao valor de 2*PI*raio e o nome
;;    "area" ao valor de PI*raio^2, onde o valor de "PI" deve ser
;;    3.14159 e "raio" deve ser associado a um valor digitado pelo usuário. 
;;    Calcule o valor do perímetro e da área da respectiva circuferência e, na
;;    sequência, calcule também o perímetro e a área de uma circuferência com 
;;    raio igual à raiz de 2.
(display "\n--------Tarefa 2--------\n")

(define raio (sqrt 2))
(define perimetro (* 2 pi raio))
(define area (* pi (expt raio 2)))
(display "Perimetro: ") perimetro
(display "Area: ") area
;;
;; 3. Escreva um procedimento (calcula-hipotenusa Ca Co), onde os parâmetros  
;;    representam respectivamente o cateto adjacente e o oposto de um triângulo  
;;    retângulo. Esse procedimento deve calcular a hipotenusa através do Teorema  
;;    de Pitágoras, usando uma função auxiliar (soma-dos-quadrados a b).
(display "\n--------Tarefa 3--------\n")

(define (soma-dos-quadrados a b) (+ (* a a) (* b b)))
(define (calcula-hipotenusa Ca Co) (sqrt (soma-dos-quadrados Ca Co)))

(display "Hipotenusa: ")
(calcula-hipotenusa 8 6)
;;
(display "\n--------Tarefa 4--------\n")
;; 4. Complete as definições das funções media, perto?, bom? e melhora, 
;;    e utilize-as de modo que a função raiz calcule a raiz quadrada de 
;;    acordo com o método de Heron de Alexandria.


(define (media a b) (/ (+ a b) 2))
; media(a, b) = (a + b) / 2

(define (perto? a b) (< (abs (- a b)) 0.001))
; |a - b| < 0.001 ?

(define (bom? chute x) (perto? (* chute chute) x))
; |chute^2 - x| < 0.001 ? (usar a função "perto?")

(define (melhora chute x) (media chute (/ x chute)))
; media(chute, x/chute)

(define (raiz-it chute x)
  (cond ((bom? chute x) chute)
        (else (raiz-it (melhora chute x) x))))

(define (raiz x)
  (raiz-it 1.0 x))
(display "Raiz 2: ")
(raiz 2)

(display "\n--------Tarefa 5--------\n")
;; 5. A conversão de uma temperatura em graus Fahrenheit para graus Celsius pode 
;;    ser realizada através da fórmula Tc = (Tf - 32) x 5/9. Por outro lado, para 
;;    converter uma temperatura em graus Celsius para graus Kelvin bastará somar 
;;    273.16. Escrever o procedimento (converte-temperatura T U), onde T 
;;    representa o valor de uma temperatura, enquanto U é um símbolo do  
;;    conjunto {Celsius, Fahrenheit, Kelvin} que indica a sua unidade. Esse  
;;    procedimento deve fazer a conversão de temperaturas conforme o exemplo abaixo:
;;    
;;    > (converte-temperatura 14 'Fahrenheit)
;;    Celsius: -10
;;    Fahrenheit: 14
;;    Kelvin: 263.16

(define (fahrenheit-celsius T) (/ (* (- T 32) 5) 9))
(define (celsius-kelvin T) (+ T 273.16))
(define (celsius-fahrenheit T) (+ (/ (* T 9) 5) 32))
(define (kelvin-celsius T) (- T 273.16))
(define (converte-temperatura T U) (cond
                                     ((eq? U 'Celsius)
                                       (display "Celsius: ") T
                                       (display "\nFahrenheit: ") (celsius-fahrenheit T)
                                       (display "\nKelvin: ") (celsius-kelvin T))
                                     ((eq? U 'Fahrenheit) 
                                       (display "Celsius: ") (fahrenheit-celsius T)
                                       (display "\nFahrenheit: ") T
                                       (display "\nKelvin: ") (celsius-kelvin (fahrenheit-celsius T)))
                                     ((eq? U 'Kelvin) 
                                       (display "Celsius: ") (kelvin-celsius T)
                                       (display "\nFahrenheit: ") (celsius-fahrenheit (kelvin-celsius T))
                                       (display "\nKelvin: ") T)
                                     (else (display "Formato de temperatura incorreto"))))
;Não consegui fazer com que mostre mais de um numero, apenas mostra o ultimo valor convertido

(converte-temperatura  14 'Celsius)

;;
;;    OBS: O procedimento só deve fazer as operações de multiplicação e divisão 
;;         uma única vez.
;;
;; Links:
;;    http://www.inf.puc-rio.br/~roberto/icc/texto/icc.html#cap1
;;    http://people.csail.mit.edu/jaffer/r5rs_toc.html
;;    http://www.drscheme.org/
;;    Menu Ajuda/Diretório de Ajuda/Quick: An Introduction to PLT Scheme with Pictures 
