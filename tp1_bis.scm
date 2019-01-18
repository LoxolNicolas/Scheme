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
(mapkar sqrt '(1 5 6 8))
(mapkar (lambda (x) (+ x 3)) '(1 5 6 8))

(define (insertionNb L x)
  (if (null? L)
      (list x) 
      (if (< (car L) x)
          (cons (car L) (insertionNb (cdr L) x))
          (cons x L))))

(define (tri_InsNb L)
  (if (null? L)
      ()
      (insertionNb (tri_InsNb (cdr L)) (car L))))

(define (insertionString L x)
  (if (null? L)
      (list x) 
      (if (string<? (car L) x)
          (cons (car L) (insertionString (cdr L) x))
          (cons x L))))

(define (tri_InsString L)
  (if (null? L)
      ()
      (insertionString (tri_InsString (cdr L)) (car L))))

(tri_InsNb '(8 4 5 1 2))
(tri_InsString '("yoyo" "salut" "azerty"))

(define (insertion_Type L x f)
  (if (null? L)
      (list x) 
      (if (f (car L) x)
          (cons (car L) (insertion_Type (cdr L) x f))
          (cons x L))))

(define (tri_Ins_Type L f)
  (if (null? L)
      ()
      (insertion_Type (tri_Ins_Type (cdr L) f) (car L) f)))

(define (Comp_Pers_Taille p1 p2)
   (< (caddr p1) (caddr p2)))

(define (Comp_Pers_Age p1 p2)
  (< (cadr p1) (cadr p2)))

(define (Comp_Pers_Nom p1 p2)
  (string<? (car p1) (car p2)))

(tri_Ins_Type '(("toto" 22 180) ("titi" 10 150)) Comp_Pers_Age)

;; Tx = (f(x+h) - f(x-h))/ 2h

(define h 0.001)

(define (deriv f h)
  (lambda (x)
    (/ (- (f (+ x h)) (f (- x h))) (* 2 h))))

((deriv (lambda (x) (sqrt x))) 8)


;; fog : x -> f(g(x))

(define (fog f g)
     (lambda (x)
       (f (g x))))

((fog (lambda (x) (sqrt x)) (lambda (x) (* x x))) 5)

          
(define (mapkar f L)
  (if (null? L)
      ()
      (cons (f (car L)) (mapkar f (cdr L)))))


(map carre '(6 7 4 3))

(define mapkan
  (lambda (f L)
    (if (null? L)
        ()
        (append (f (car L)) (mapkan f (cdr L))))))

(mapkan (lambda (x)
          (if (integer? x)
              (list x)
              ())) '(a 5 2 3 t))

;; map existe deja voir cours
;; append-map


;;EXERCICE 4

(define (PC E n)
  (if (= n 0)
     '(())
     (let ((PCN1 (PC E (- n 1))))   ;; EVITE DE RECALCULER POUR CHAQUE E
         (append-map (lambda (z) (cons_each z PCN1)) E))))
     
(define (prod_cart E n)
  (if (null? E)
      ()
      (PC E n)))

(define (cons_each x L)
  (if (null? L)
      '()
     (cons (cons x (car L)) (cons_each x (cdr L)))))

(prod_cart '(0 1) 2)

;;EXERCICE 8

(cons_each 2 '((1) (2) (3)))

(define (P E)
  (if (null? E)
      '(())
      (let ((PCN1 (P (cdr E))))
        (append PCN1 (map (lambda (z) (cons (car E) z)) PCN1)))))

(P '(1 2 3))

;;EXERCICE 6

(define (retire M)
  (if (null? M)
      '()
      (cons (cdr (car L)) (retire (cdr L)))))


(define (tr M)
  (if (null? M)
      0
      (+ (caar M) (tr (map cdr (cdr M))))))

(tr '((1 2 3) (4 5 6) (7 8 9)))


(define (transposee M)
  (if (null? (car M))
      '()
      (cons (map car M) (transposee (map cdr M)))))

(transposee '((1 2 3) (4 5 6) (7 8 9)))

(map list '(1 2 3) '(4 5 6) '(7 8 9))

(define (produit_Matrice M v)
  (if (or (null? M) (null? v))
      '()
      (map (lambda(z) (apply + (map * z v))) M)))


;;EXERCICE SR

(define SR (lambda (L I R)
             (if (null? L) I
                 (R (car L) (SR (cdr L) I R)))))


(define (max L)
  (if (null? L)
      "Liste vide"
    (SR (cdr L) (car L) (lambda (Tete RR) ;;RR = resultat sur le reste
                    (if (> Tete RR)
                        Tete
                        RR)))))

(max '(1 5 6 9 8))

;;EXERCICE 1A

(define existe?
  (lambda (L P)
    (if (null? L)
        #f
        (or (P (car L))
            (existe? (cdr L) P)))))

(define (tous_egaux_exi? L)
  (not (existe? (cdr L) (lambda (z)
               (not (= (car L) z))))))

(tous_egaux_exi? '(5 5 5))

;;EXERCICE 1B

(define quel_que_soit?
  (lambda (L P)
    (if (null? L)
        #t
        (and (P (car L))
             (quel_que_soit? (cdr L) P)))))

(define (tous_egaux_uni? L)
  (quel_que_soit? (cdr L) (lambda (z)
                            (= (car L) z))))

(tous_egaux_uni? '(7 7 7))

;;EXERCICE 2.1

(define (rec U0 UN n)
  (if (= n 0)
      U0
      (UN (rec U0 UN (- n 1)) n)))

(rec 0 (lambda (UN_1 n) (+ UN_1 n) ) 4)

;;EXERCICE 3

(define (reflexive E R)
  (quel_que_soit? E (lambda (x)
                      (R x x))))

(define (symetrique E R)
                   (quel_que_soit? E
                                   (lambda (x)
                                       (quel_que_soit? E
                                        (lambda (y)
                                         (or (not (R x y)) (R y x)))))))

(symetrique '(1 2 3) =)

(define (transitive E R)
                   (quel_que_soit? E
                                   (lambda (x)
                                       (quel_que_soit? E
                                        (lambda (y)
                                          (quel_que_soit? E
                                                          (lambda (z)
                                                                            (or (not (R x y)) (not (R y x)) (R x y))))))))))

                                         


  
                         

                    







