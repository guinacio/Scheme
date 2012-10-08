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

;; ---------- lambda - let - define -------------------

(define (mais4 x) (+ x 4))
(mais4 3)

;; LAMBDA
;(lambda           (x)                (+        x   4))
; o procedimento   de um argumento x   que soma x e 4
((lambda (x) (+ x 4)) 3)

(define soma4 (lambda (x) (+ x 4)))

;; LET
(let ((x 3)) (+ x 4))

; Três procedimentos para calcular -xy, com x=5 e y=7:

(define (f x y) (* -1 x y))
(f 5 7)

((lambda (x y) (* -1 x y)) 5 7)

(let ((x 5) (y 7)) (* -1 x y))

;; ----------------------------------------------------

;; PROCEDIMENTOS DE ORDEM SUPERIOR:

(define (soma a b termo prox)
  (cond ((> a b) 0)
	(else (+ (termo a)
	         (soma (prox a) b termo prox)))))


(define (integral f a b dx)
  (* (soma (+ a (/ dx 2.0))
           b
           f
           (lambda (x) (+ x dx)))
     dx))

(integral (lambda (x) (* x x x)) 0 1 0.001)

;; SUA TAREFA:
;;
;; OBS.: Procure utilizar as chamadas lambda e let.
;;
;; 1. Utilizando o procedimento "soma", programar as seguintes funções:
;;
;;    * Soma dos números pares entre a e b (inclusive).
;;
(define (soma-pares a b)
  (soma a
        b
        (lambda (x) (if (even? x) x 0))
        (lambda (x) (+ x 1))))
(soma-pares 1 7)

;;    * Função seno utilizando a série (o número de termos deve ser um
;;      parâmetro da função):
;;
;;      x - x^3/3! + x^5/5! - x^7/7! + ...
;;
(define (fat n)
  (define (fat-it n p)
    (if (zero? n)
        p
        (fat-it (- n 1) (* n p))))
  (fat-it n 1))

(define (seno x n)
  (soma 1
        n
        (lambda (y) 
          (let ((k (- (* 2 y) 1)))
            (* (expt -1 (- y 1)) (/ (expt x k) (fat k)))))
        (lambda (y) (+ y 1))))

(seno (/ 3.141592 2) 100)

;; 2. Reprogramar o procedimento "soma" de modo a gerar um processo iterativo.
;;
(define (soma-i a b termo prox)
  (define (soma-it a b termo prox s)
    (cond ((> a b) s)
          (else (soma-it (prox a) b termo prox (+ (termo a) s)))))
  (soma-it a b termo prox 0))

(soma-i 1 5 (lambda (x) x) (lambda (x) (+ x 1)))

;; 3. Programar um procedimento "produtório", análogo ao somatório.
;;
;; Produtorio Recursivo
(define (produtorio a b termo prox)
  (cond ((> a b) 1.0)
        (else (* (termo a) (produtorio (prox a) b termo prox)))))

;; Produtorio Iterativo  
(define (produtorio-it a b termo prox)
  (define (produtorio-ir a b termo prox result)
    (cond ((> a b) result)
          (else (produtorio-ir (prox a) b termo prox (* (termo a) result)))))
  (produtorio-ir a b termo prox 1.0))

;; 4. Utilizando o procedimento "produtório" programar um procedimento que
;;    calcule PI utilizando a fórmula (o número de termos deve ser um
;;    parâmetro da função):
;;
;;    pi = 4 * (2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 * ...)
;;
(define (pi2 n)
  (* 4
     (produtorio-it 1
                    n
                    (lambda (i)
                      (cond ((even? i) (/ (+ i 2) (+ i 1)))
                            (else (/ (+ i 1) (+ i 2)))))
                    (lambda (i) (+ i 1)))))

(pi2 1000000)

;; 5. Considere a regra de Simpson para cálculo numérico da integral de uma
;;    função f entre a e b:
;;
;;    (h/3)* [y0 + 4 y1 + 2 y2 + 4 y3 + 2 y4 +...+ 2 yn-2 + 4 yn-1 + yn]
;;
;;    onde h = (b - a)/n, para n par e yk = f(a + k h).
;;
;;    Programe uma função que recebe f, a, b e n e retorna o valor da
;;    integral. Compare com o resultado com o da função integral.
;;

(define (intS f a b n)
  (let ((h (/ (- b a) n)))
    (* (/ h 3.0)
       (soma-i 0
               n
               (lambda (k) 
                 (let ((t (f (+ a (* k h)))))
                   (cond ((or (= k 0) (= k n)) t)
                         ((even? k) (* 2 t))
                         (else (* 4 t)))))
               (lambda (k) (+ k 1))))))

(define (cubo x)
  (* x x x))

(integral cubo 0 1 (/ 1 10))
(intS cubo 0 1 10)

;; 6. Implemente o procedimento de ordem superior "integral" na linguagem C.
;;

; No arquivo labA5max.c