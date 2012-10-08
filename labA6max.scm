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
;;;   Programador(es): Max Hering de Queiroz
;;;
;;;
;;;   Tel.: 0xx48 3721 7791
;;;   E-mail: max@das.ufsc.br

;; Constantes

(define *erro* 0.001)


(define *dx* 0.001)

;; Funções auxiliares (reutilizadas !)

(define (perto? chute x)
  (< (abs (- chute x)) *erro*))


(define (media x y)
  (/ (+ x y) 2))


(define (quadrado x)
  (* x x))

;; Raiz quadrada: Método de Heron de Alexandria

(define (bom? chute x)
  (perto? (quadrado chute) x))


(define (melhora chute x)
  (media chute (/ x chute)))


(define (raiz-it chute x)
  (cond ((bom? chute x) chute)
	(else (raiz-it (melhora chute x) x))))

(define (raiz-hr x)
  (raiz-it 1.0 x))

;; Ponto-fixo

(define (pf f chute)
  (let ((prox (f chute)))
    (cond ((perto? chute prox) prox)
	  (else (pf f prox)))))

;; Abafamento por média

(define (ma f)
  (lambda (x)(media x (f x))))

;; Raiz quadrada: Método de ponto-fixo

(define (raiz-pf x)
  (pf (ma (lambda (y)(/ x y))) 1.0))

;; Método de Newton

(define (newt g)
  (lambda (x)(- x (/ (g x)
		     ((deriv g) x)))))


(define (deriv g)
  (lambda (x)(/ (- (g (+ x *dx*)) (g x))
		*dx*)))


(define (newmeth g chute)
  (pf (newt g) chute))

;; Raiz quadrada: Método de Newton

(define (raiz-mn x)
  (newmeth (lambda (y)(- (quadrado y) x)) 1.0))

(raiz-mn 2)

;; SUA TAREFA:
;;
;; 1. Escrever uma função "pf-trans" que recebe uma função, uma
;;    transformada e um chute e calcula o ponto-fixo da transformada da
;;    função. Reescrever os métodos de ponto-fixo e de Newton para raiz
;;    quadrada utilizando esta função.
;;

(define (pftrans g trans chute)
  (pf (trans g) chute))

(define (raiz-pftma x)
  (pftrans (lambda (y) (/ x y)) ma 1.0))

(raiz-pftma 2)

(define (raiz-pftmn x)
  (pftrans (lambda (y) (- (quadrado y) x)) newt 1.0))

(raiz-pftmn 2)

;; 2. Escrever funções para a raiz enésima usando os métodos de ponto-fixo
;;    e de Newton.
;;

(define (exp-l b n)
  (cond ((= n 0) 1)
        ((even? n) (quadrado (exp-l b (/ n 2))))
        (else (* b (exp-l b (- n 1))))))

(define (raizn-pftma x n)
  (pftrans (lambda (y) (/ x (exp-l y (- n 1)))) ma 1.0))

(raizn-pftma 1024 10)

(define (raizn-pftmn x n)
  (pftrans (lambda (y) (- (exp-l y n) x)) newt 1.0))

(raizn-pftmn 2048 11)

;;
;; 3. A expressão:
;;
;;        N(1)
;;    -----------
;;    D(1) + N(2)
;;           -----------
;;           D(2) + N(3)
;;                  -----------
;;                  D(3) + N(4)
;;                         ----------
;;                         ... + N(k)
;;                               ----
;;                               D(k)
;;
;;    é chamada de fração contínua truncada em k. Escreva uma função "fc"
;;    que calcule esta expressão recebendo como parâmetros as funções
;;    N(i) e D(i) e o limite k.
;;    Caso a função esteja correta o valor da chamada:
;;
;;     (fc (lambda (x) 1.0) (lambda (x) 1.0) k)
;;
;;    com k um inteiro positivo deve retornar uma aproximação para o
;;    inverso da razão áurea: 1/phi=1/1,6180=0,6180.
;;

(define (fc N D k) 
  (fc-iter N D k 1))

(define (fc-iter n d k2 i)
  (if (= i k2)
      (d k2)
      (/ (n i) (+ (d i) (fc-iter n d k2 (+ i 1))))))

(fc (lambda (x) 1.0) (lambda (x) 1.0) 100)

;; 4. Utilize a função "fc" para calcular a tangente usando a seguinte
;;    fração contínua, proposta por J.H. Lambert em 1770:
;;
;;                 x
;;    tag(x) = ---------
;;                    x^2
;;             1 - ---------
;;                        x^2
;;                 3 - ---------
;;                            x^2
;;                     5 - ---------
;;                            ...

(define (tg x k)
  (/ (fc (lambda (i) (* x x -1.0))
         (lambda (i) (- (* 2 i) 1))
         k)
     (* -1 x)))

(tg 3.141592 100)