;; TD1 SCHEME


;;EXERCICE 1

(define carre
  (lambda(x)
    (* x x)))

(carre 4)

;;EXERCICE 2

(define quadV1
  (lambda(x)
    (carre (carre x)))) 

(define quadV2
  (lambda(x)
    (* (carre x) (carre x))))

(quadV1 5)
(quadV2 5)

;;EXERCICE 3

(define pi 3.14)

(define cercle
  (lambda(r)
   (list  (* 2 pi r)
    (* pi (carre r)))))

(cercle 4)

;;EXERCICE 4

(define LA
  '(1 (2 (6)
         (7))
      (3 (8 (12))
         (9))
      (4(10 (13)
            (14 (17)))
        (11 (15 (16)))
        (18))
      (5)))

(car LA) ;;La tete de l'arbre

(cdr LA) ;;La liste des sous arbre de l'arbre principale

(car (cdr LA)) ;;Sous arbre de racine 2
;;ou
(cadr LA)

(car (cdr (cdr (cdr LA))))
;ou
(cadddr LA)

((caadr (cadr (caddr LA))))


;;EXERCICE

(define (ajouter_fin x L)
  (if (null? L)
      (list x)
      (cons (car L) (ajouter_fin x (cdr L)))))

(ajouter_fin 8 '(2 5 6))

(define (membre x L)
  (if (null? L)
      #f
      (if (= x (car L))
          #t
          (membre x (cdr L)))))

(membre 5 '(1 5 3))

(define (epure L)
  (if (null? L)
      ()
      (if (membre (car L) (cdr L))
          (epure (cdr L))
          (cons (car L) (epure (cdr L))))))

(epure '(2 1 3 2 1 5))

(define (miroir L)
  (if (null? L)
      ()
      (append (miroir (cdr L)) (list (car L)))))

(define (epureRT L Lres)
  (if (null? L)
      (miroir Lres) ;;Pour avoir la liste dans l'ordre
      (if (membre (car L) Lres)
          (epureRT (cdr L) Lres)
          (epureRT (cdr L) (cons (car L) Lres)))))

(epureRT '(5 3 4 5 8 9) '())

(define (nieme n L)
  (if (null? L)
      'erreur
      (if (= n 0)
          (car L)
          ;;SINON
          (nieme (- n 1) (cdr L)))))

(nieme 3 '(2 1 3 5))

(define (union L1 L2)
  (if (null? L1)
      L2
      (if (membre (car L1) L2)
          (union (cdr L1) L2)
         ;;SINON
          (cons (car L1) (union (cdr L1) L2)))))

(define (union_Bis L1 L2)
  (epure (append L1 L2)))

(union '(1 2 5 4) '(5 4 3 8 0))
(union_Bis '(1 2 5 4) '(5 4 3 8 0))

(define (inter L1 L2)
  (if (null? L1)
      ()
      (if (membre (car L1) L2)
          (cons (car L1) (inter (cdr L1) L2))
          (inter (cdr L1) L2))))

(inter '(1 2 5 4) '(5 4 3 8 0))

(define (niv0 L)
  (if (null? L)
      ()
      (if (list? (car L))
          (niv0 (append (niv0 (car L)) (cdr L)))
          (cons (car L) (niv0 (cdr L))))))

(niv0 '(((2 3) (5))))


(define (zip L1 L2)
  (if (or (null? L1) (null? L2))
      ()
      ())) ;; A FINIR

;;ecrire mapkar fct L
;;retourne ( f(e1) ,... )
;; appliquer a sqrt
;; appliquer au carre
;; appliquer a +3

(define (mapkar f L)
  (if (null? L)
      ()
      (cons (f (car L)) (mapkar f (cdr L)))))

(mapkar carre '(1 5 6 8))



          
                  
  





