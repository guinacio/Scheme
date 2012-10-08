#lang scheme
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;   Linguagem: Essentials of Programming Languages (2nd ed.) costumizado.
;;;
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102
;;;
;;;   Programador(es): Guilherme Silva Inácio
;;;   Email:
;;;


;;; Manipulação de listas


;; Enumerar:
(define (enumerar n1 n2)
  (if (> n1 n2)
      (list)
      (cons n1 (enumerar (+ n1 1) n2))))

(display  "Teste da funcao enumerar:\n") 
(enumerar 0 10) 
(newline)

;; Enésimo:
(define (enesimo n lista)
  (cond ((zero? n) (car lista))
        (else (enesimo (- n 1) (cdr lista)))))

(display  "Teste da funcao enesimo:\n")
(define lista-grande (enumerar 0 200000))
(time (enesimo 100000 lista-grande))
(newline)


;; ------------------- Parte 1 ----------------------

;; a) Número de elementos:
;;
;; entrada: <lista>
;;
;; saída: <número de elementos da lista>
;;
;; exemplo: (numel (list 1 -2 3 0)) -> 4

(define (numel l)
  (cond ((null? l) 0)
	(else (+ 1 (numel (cdr l))))))

(display  "Teste da funcao numel:\n")
(time (numel lista-grande))
(newline)

(define (numel-it l) 
  (numel-aux l 0))
(define (numel-aux l cont) 
  (cond ((null? l) cont)
        (else (numel-aux (cdr l) (+ cont 1)))))
(display "Numel-it: ")
(numel-it lista-grande)


;; b) Inverso:
;;
;; entrada: <lista>
;;
;; saída: <lista com elementos na ordem inversa à ordem da
;;         lista de entrada>
;;
;; exemplo: (inverte (list 1 -2 3 0)) -> (0 3 -2 1)

(define (inverte l) 
    (if (null? l) l
    (cons (last l) (inverte (drop-right l 1)))))
(display "Inverte-r: ")
(inverte (list 1 -2 3 0))



(define (inverte-it l) 
  (inverte-itaux l null))
(define (inverte-itaux l nova)
  (cond
    ((null? l) nova)
    (else (inverte-itaux (cdr l) (cons (car l) nova)))))

(display "Inverte-it: ")
(inverte-it (list 1 -2 3 0))

;; ------------------- Parte 2 ----------------------

;; c) Concatenação:
;;
;; entrada: <lista> <lista>
;;
;; saída: <lista cujos elementos são o resultado da concatenação
;;         entre as duas listas>
;;
;; exemplo: (junta (list 3 4) (list 1 2 3)) -> (3 4 1 2 3)

(define (junta l1 l2) (junta-aux (inverte-it l1)l2 l2))
(define (junta-aux l1 l2 nova)
  (cond 
    ((null? l1) nova)
    (else (junta-aux (cdr l1) nova (cons (car l1) nova)))))

(display "Junta: ")
(junta (list 3 4) (list 1 2 3))
;; d) Mapear: 
;;
;; entrada: <função> <lista>
;;
;; saída: <lista cujos elementos são o resultado da aplicação da
;;         função dada nos elementos da lista original>
;;
;; exemplo: (mapear abs (list -1 2 -3)) -> (1 2 3)

(define (mapear func lista) (mapear-aux func (inverte-it lista) null))
(define (mapear-aux func l1 nova)
  (cond
    ((null? l1) nova)
    (else (mapear-aux func (cdr l1) (cons (func (car l1)) nova)))))

(display "Mapear: ")
(mapear abs (list -1 2 -3))

;; e) Filtrar: 
;;
;; entrada: <função do tipo predicado (retorna #t ou #f)> <lista>
;;
;; saída: <lista que contém os elementos da lista original para os
;;         quais a aplicação da função predicado dada resulta #t>
;;
;; exemplo: (filtrar even? (list 1 2 3 4)) -> (2 4)

(define (filtrar pred lista) (filtrar-aux pred (inverte-it lista) null))
(define (filtrar-aux pred l1 nova)
  (cond
    ((null? l1) nova)
    ((pred (car l1)) (filtrar-aux pred (cdr l1) (cons (car l1) nova)))
    (else (filtrar-aux pred (cdr l1) nova))))

(display "Filtrar: ")
(filtrar even? (list 1 2 3 4))

;; ------------------- Parte 3 ----------------------

;; f) Acumular: 
;;
;; entrada: <função de duas variáveis> <valor inicial> <lista de valores>
;;
;; saída: <valor resultante da aplicação progressiva da função a cada 
;;         valor da lista e ao resultado da operação anterior, partindo 
;;         do valor inicial>
;;
;; exemplo: (acumular + 0 (list 1 2 3 4)) -> 10

(define (acumular op inicial lista)
  (if (null? lista)
      inicial
      (op (car lista)
          (acumular op inicial (cdr lista)))))

(display  "Teste da funcao acumular: ")
(acumular + 0 (list 1 2 3 4))

;; g) Produto do quadrado dos números ímpares:
;;
;; entrada: <número>
;;
;; saída: <produto do quadrado de todos os números ímpares 
;;         entre 0 e o número dado>
;;
;; exemplo: (prod-quad-imp 7) -> 11025

(define quad (lambda (x) (* x x)))

(define (prod-quad-imp n) (prod-quad-imp-aux (enumerar 0 n)))
(define (prod-quad-imp-aux l1)
 (acumular * 1 (mapear quad (filtrar odd? l1))))

(display "Prod-quad-imp: ")
(prod-quad-imp 7)

;; --------------------------------------------------

;; SUA TAREFA:
;;
;; 1. Programar recursivamente E iterativamente as funções de
;;    manipulação de listas na Parte 1 acima (itens a e b).
;;
;; 2. Programar recursivamente OU iterativamente as funções de
;;    manipulação de listas na Parte 2 acima (itens c, d e e).
;;
;; 3. Usar os procedimentos enumerar, mapear, filtrar e acumular
;;    para implementar uma função (prod-quad-imp n) que calcule 
;;    o produto do quadrado de todos os números ímpares entre 0 
;;    e n (item g da Parte 3).
;;
;; 4. Resolver as tarefas 1, 2 e 3 acima na linguagem C.
