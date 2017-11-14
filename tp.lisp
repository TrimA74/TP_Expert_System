;SEBIRE branche



(defun put(symbol prop value) ; ajouter une propriété dans un objet 'symbol'
	(self (get symbol prop) value))
; création d'une règle
(defun newrule (symb cond act)
	(cond ( (member symb LR) (print ‘Already_used_name) nil) ; si la rule est dans LR on fait rien
		( t (setq LR (cons symb LR)) ; sinon on l'ajoute symb dans LR
			(put symb ‘condition cond) ; ajouter une propriété appellée : 'condition' dans lequel on met cond
			(put symb ‘action act) ; pareil
			(put symb ‘appliquee nil) ; pareil
			symb ))) ; return la rule (object symb)
; suppresion d'une règle
(defun deleterule (symb)
	(cond ( (member symb LR) (setq LR (remove symb LR)) symb) ; si 
		( t (print ‘The_rule_does_not_exist) nil))) 

; on affiche une règle
(defun printrule (symb)
(cond ( (null (member symb LR)) (print ‘The_rule_does_notè_exist) nil)
	( t (print symb)
	(prin1 ‘ condition :’)(print (get symb condition))
	(prin1 ‘ action :’)(print (get symb action))
	symb ))) 
; Tester si une règle est applicable 
(defun applicable (x)
 (eval (get x ‘condition ) )) 

; Appliquer une règle 
 $ (defun appliquer (x)
 (eval (get x ‘action))
 (put x ‘appliquee t) ) 

 ;Savoir si une règle a déjà été appliquée
 (defun appliquee (x)
 	(get x ‘appliquee) ) 
; Ensemble de conflits (liste des règles activables) 
 (defun conflictSet (ll) ; ll variable locale
 (setq ll nil)
(mapc ‘(lambda (x) (cond ( (and (not (appliquee x)) (applicable x))
 (setq ll (cons x ll)) ) ))
 LR)
ll ) 
; running expert system
 (defun run ()
 (cond ( (eval CA) (print ‘Fin_Condition_d_arret_verifiée) t )
 ( (null (conflictset)) (print ‘Pb_non_resolu_et_plus_de_Regles_Activables) nil )
( t (mapc ‘appliquer (conflictset)) ; on applique toutes les règles
 (run) ))) 


; Exemple
; ------------
; Base de faits
; ------------------
(setq Temp 37)
(setq fortefievre nil)
; Base de règles
; ---------------------
( setq LR nil ) ; LR liste des règles 
(newrule 'r1 '(> Temp 38) '(setq fortefievre T))
(newrule 'r2 T ’(progn (prin1 'temperature?) (setq Temp (read))))
; Condition d arrêt
; ------------------------
(setq CA '(not (null fortefievre)))
; Lancement du moteur
; -------------------------------
(run) 































(defun longueur (l)
	(cond
			((null l) 0 ) ; si l est null on renvoie 0
			((atom l) nil) ; si l est un atom on renvoie nil
			( t ( + (longueur ( cdr l)) 1 )) ; sinon on renvoie longueur du cdr de la liste + 1 
	)
)

(defun longueurPlus (l)
	(cond
			((null l) 0 )
			((atom l) nil)
			( (listp ( car l)) ( + (longueurPlus (car l) (longueurPlus (cdr l) )) ) ) ; si car l est une liste on renvoie la longueur de car + cdr de l 
			( t ( + (longueur ( cdr l)) 1 ))
	)
)

(defun isIn (l e) 
	(cond 
		((atom l ) nil)
		((equal e (car l)) t) ; si e = car l 
		(t (isIn (cdr l) e)) ; sinon isIn 
	)
)

(defun munion ( l1 l2 )
	(cond
		((null l2) l1)
		((null l1) l2)
		((atom l2) (cond 
						( (isIn l1 l2) l1)
						(t append l1 l2)
					))
		((atom l1) (cond 
						( (isIn l2 l1) l2)
						(t append l2 l1
					)))
		((isIn l1 (car l2)) (munion l1 (cdr l2)) )
		(t (cons (car l2) (munion (cdr l2) l1 )) )
	)
)
; marche pas 
(defun inter ( l1 l2 )
	(cond
		( (not (and  (listp l1) (listp l2) ) ) nil ) 
		( (null l1 ) nil)
		( ( member l1 (car l2) ) ( cons (car l2) (inter l1 (cdr l2) ) ) )
		( t (inter l1 (cdr l2) ) ) 
	)
)
(defun my-intersection ( liste1 liste2 )
  (cond
       ( (not (and (listp liste1) (listp liste2))) nil)
       ( (null liste1) nil)
       ( (member (car liste1) liste2) ( cons (car liste1) (my-intersection (cdr liste1) liste2) ))
       (t (my-intersection (cdr liste1) liste2))
  )
)
(defun fin (l)
	(cond
		((null l) nil )
		( (null (cdr l) ) (car l) )
		( t (fin (cdr l)) )
	) 
)

(defun inverse (l)
	(cond
		( () () )
		(t () )


	)


)

(defun renverse (liste)
	(cond 
		( (atom liste) liste)
 	( t (append (renverse (cdr liste))
 		(list (renverse (car liste)))

 	))



 	)


) 
