;SEBIRE branche



(defun put(symbol prop value) ; ajouter une propriété dans un objet 'symbol'
	(self (get symbol prop) value))
; création d'une règle
(defun newrule (symb cond act)
	(cond ( (member symb LR) (print 'Already_used_name) nil) ; si la rule est dans LR on fait rien
		( t (setq LR (cons symb LR)) ; sinon on l'ajoute symb dans LR
			(put symb 'condition cond) ; ajouter une propriété appellée : 'condition' dans lequel on met cond
			(put symb 'action act) ; pareil
			(put symb 'appliquee nil) ; pareil
			symb ))) ; return la rule (object symb)
; suppresion d'une règle
(defun deleterule (symb)
	(cond ( (member symb LR) (setq LR (remove symb LR)) symb) ; si la rule est dans LR on la remove
		( t (print 'The_rule_does_not_exist) nil))) ; sinon rien

; on affiche une règle
(defun printrule (symb)
(cond ( (null (member symb LR)) (print 'The_rule_does_notè_exist) nil) ; si règle existe pas on fait rien
	( t (print symb) ; sinon on l'affiche
	(print ' condition :')(print (get symb condition)) ; on affiche la condition
	(print ' action :')(print (get symb action)) ; on affiche l'action
	symb )))  ; retourne la rule


; Tester si une règle est applicable 
(defun applicable (x)
 (eval (get x 'condition ) )) ; on évalue la condition, retourn bool


; Appliquer une règle 
 $ (defun appliquer (x) ;@x une règle
 (eval (get x 'action)) ; on évalue l'action de x
 (put x 'appliquee t) ) ; mets true dans la propriété appliquee de @x


 ;Savoir si une règle a déjà été appliquée
 (defun appliquee (x)
 	(get x 'appliquee) ) ; récupère la propriété appliquée


; Ensemble de conflits (liste des règles activables) 
 (defun conflictSet (listRule) ; listRule (variable locale)
	(setq listRule nil)
	(mapc ; applique la fonction lambda sur toutes les rules dans LR
		'(lambda (x) 
			(cond ( (and (not (appliquee x)) (applicable x)) ; si rule n'est pas appliquée mais applicable
				(setq listRule (cons x listRule)) ) )) ; mets la rule dans listRule
	LR)
	listRule ) ; retourne liste de rule qui n'ont pas été appliquée mais applicable


; running expert system
 (defun run ()
	(cond ( (eval CA) (print 'Fin_Condition_d_arret_verifiée) t ) ; si CA == true c'est la fin 
		( (null (conflictset)) (print 'Pb_non_resolu_et_plus_de_Regles_Activables) nil ) ; si toute les règles ont été appliquées et qu'on n'a plus de règle applicable ça marche po
		( t (mapc 'appliquer (conflictset)) ; sinon on applique toutes les règles
		(run) ))) ; on rappelle run 


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
(newrule 'r2 T '(progn (prin1 'temperature?) (setq Temp (read))))
; Condition d arrêt
; ------------------------
(setq CA '(not (null fortefievre)))
; Lancement du moteur
; -------------------------------
(run) 




















