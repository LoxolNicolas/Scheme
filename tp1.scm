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




