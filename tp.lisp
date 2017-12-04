;fact base


(defstruct waste
  material
  volume
  dirt_type
  object_type
)

(defstruct selectiveTrash 
  container_list
    
)

(defstruct container
	color 
  max_capacity
  waste_amount
 )

;rule base

;init variables
(defvar green_container)
( setq green_container (make-container
  	:color 'green
  	:max_capacity 100
    :waste_amount 0
  )
)
(defvar yellow_container)
( setq yellow_container (make-container
  	:color 'yellow
  	:max_capacity 100
    :waste_amount 0
  )
)
(defvar default_container)
( setq default_container (make-container
  	:color 'grey
  	:max_capacity 100
    :waste_amount 0

  )
)

(defvar blue_container)
( setq blue_container (make-container
  	:color 'blue
  	:max_capacity 100
    :waste_amount 0

  )
)


;our waste
;possibility for properties:
;material: glass, paper, plastic, metal
;dirt_type: clean, chimic_liquid, grease
;object_type: bag, bottle
(defvar waste1)
( setq waste1 (make-waste 
  	:material 'plastic
    :volume 1
    :dirt_type 'clean
	:object_type 'bottle
  )
) 

(defvar containerChosen)
(setq containerChosen nil)
(defvar stopCondition)
(defvar rulesList)


;Rule list
(defvar LR)
(setq LR nil)



; Définition des structures
(defstruct rule name condition action weight applied)

; Fonctions de manipulations
(defun newrule(myname mycondition myaction myweight)
    (push (make-rule ; comme le new en POO
               :name myname
               :condition mycondition
               :action myaction
               :weight myweight
               :applied nil
          )
    LR)
)

;(defun put (symbol prop value)     ;marche pas
;    (setf (get symbol prop) value))    

(defun myApply (x) ;@x une règle
    (eval (rule-action x)) ; on évalue l'action de x
    (setf (rule-applied x) t) ;(put x 'applied t) ) ; mets true dans la propriété appliquee de @x
)

; Tester si une règle est applicable 
(defun applicable (x)
    ;(print (rule-condition x))
    (eval (rule-condition x))                                   ;(get x 'condition ) )) ; on évalue la condition  retourn bool
)    

;Savoir si une règle a déjà été appliquée
(defun applied (x)
    (rule-applied x)
    ;(get x 'applied) 
) ; récupère la propriété appliquée

; findActivable
(defun findActivable ()
        (setq rulesList nil)
        (mapc 
            (lambda (x)

                ;(print (applied x))
                ;(print (applicable x))
                (cond
                    ((and (null (applied x)) (applicable x))
                    (setq rulesList (cons x rulesList)))
                )
                
            )
        LR)
        ;(print rulesList)
        rulesList
)
;to get rule by name
(defun findRuleByName( myname)
    (setq r nil)
    (mapc
        (lambda (x)
            (cond 
                ((eq (rule-name x) myname) (setq r x) ) 
            )
        )
    LR)
    r

)


; adding rules

;paper case

; r1 
;; condition -> si waste.material == paper 
;; action -> null
;; weight 1
(newrule 'r1 '(eq  (waste-material waste1) 'paper) '(print "r1 done") 1)

; r2
;; condition -> si waste.dirt_type == clean && r1.applied==true
;; action -> mettre dans la blue_container
;; weight 1
(newrule 'r2 '(and (eq (waste-dirt_type waste1) 'clean ) (applied (findRuleByName 'r1))) '(setq containerChosen 'blue_container) 1)

; r3
;; condition -> si waste.dirt_type != clean && r1.applied==true
;; action -> mettre dans la default_container
;; weight 1
(newrule 'r3 '(and (not (eq (waste-dirt_type waste1) 'clean )) (applied (findRuleByName 'r1))) '(setq containerChosen 'default_container) 1)

;glass case


; r4 
;; condition -> si waste.material == glass 
;; action -> null
;; weight 1
(newrule 'r4 '(eq  (waste-material waste1) 'glass) '(print "r4 done") 1)

; r5
;; condition -> si waste.dirt_type == grease && r4.applied==true
;; action -> mettre dans la default_container
;; weight 1
(newrule 'r5 '(and (eq (waste-dirt_type waste1) 'grease ) (applied (findRuleByName 'r4))) '(setq containerChosen 'default_container) 1)

; r6
;; condition -> si waste.dirt_type != chimic_liquid && r4.applied==true
;; action -> mettre dans la default_container
;; weight 1
(newrule 'r6 '(and (eq (waste-dirt_type waste1) 'chimic_liquid ) (applied (findRuleByName 'r4))) '(setq containerChosen 'default_container) 1)

; r7
;; condition -> si waste.dirt_type != clean && r4.applied==true
;; action -> mettre dans la green_container
;; weight 1
(newrule 'r7 '(and (eq (waste-dirt_type waste1) 'clean ) (applied (findRuleByName 'r4))) '(setq containerChosen 'green_container) 1)


;plastic case

; r8
;; condition -> si waste.material == plastic 
;; action -> null
;; weight 1
(newrule 'r8 '(eq  (waste-material waste1) 'plastic) '(print "r8 done") 1)

; r9
;; condition -> si waste.dirt_type == grease && r8.applied==true
;; action -> mettre dans la default_container
;; weight 1
(newrule 'r9 '(and (eq (waste-dirt_type waste1) 'grease ) (applied (findRuleByName 'r8))) '(setq containerChosen 'default_container) 1)

; r10
;; condition -> si waste.dirt_type != chimic_liquid && r8.applied==true
;; action -> mettre dans la default_container
;; weight 1
(newrule 'r10 '(and (eq (waste-dirt_type waste1) 'chimic_liquid ) (applied (findRuleByName 'r8))) '(setq containerChosen 'default_container) 1)

; r11
;; condition -> si waste.dirt_type != clean && r8.applied==true
;; action -> mettre dans la yellow_container
;; weight 1
(newrule 'r11 '(and (eq (waste-dirt_type waste1) 'clean ) (applied (findRuleByName 'r8))) '(print "r11 done") 1)

; r10
;; condition -> si waste.dirt_type != chimic_liquid && r8.applied==true
;; action -> mettre dans la default_container
;; weight 1
(newrule 'r10 '(and (eq (waste-object_type waste1) 'bag ) (applied (findRuleByName 'r11))) '(setq containerChosen 'default_container) 1)

; r11
;; condition -> si waste.dirt_type != clean && r8.applied==true
;; action -> mettre dans la yellow_container
;; weight 1
(newrule 'r11 '(and (eq (waste-object_type waste1) 'bottle ) (applied (findRuleByName 'r11))) '(setq containerChosen 'yellow_container) 1)


; metal case

; r12
;; condition -> si waste.material == metal 
;; action ->  mettre dans la yellow_container
;; weight 1
(newrule 'r12 '(eq  (waste-material waste1) 'metal) '(setq containerChosen 'yellow_container) 1)

; carton case

; r13
;; condition -> si waste.material == carton 
;; action ->  null
;; weight 1
(newrule 'r13 '(eq  (waste-material waste1) 'carton) '(print "r13 done") 1)

; r14
;; condition -> si waste.dirt_type == clean && r13.applied==true
;; action -> mettre dans la yellow_container
;; weight 1
(newrule 'r14 '(and (eq (waste-dirt_type waste1) 'clean ) (applied (findRuleByName 'r13))) '(setq containerChosen 'yellow_container) 1)

; r15
;; condition -> si waste.dirt_type != clean && r13.applied==true
;; action -> mettre dans la default_container
;; weight 1
(newrule 'r15 '(and (not (eq (waste-dirt_type waste1) 'clean )) (applied (findRuleByName 'r13))) '(setq containerChosen 'default_container) 1)
















;stop condition
(setq stopCondition
	'(cond
		
		((or (<= (container-max_capacity green_container) (container-waste_amount green_container))
			(<= (container-max_capacity blue_container) (container-waste_amount blue_container))
			(<= (container-max_capacity yellow_container) (container-waste_amount yellow_container))
            (not (null containerChosen))  ;on s'arrete quand la poubelle choisi est défini
                                            ;poubelle peut etre blue, yellow, green...ou other(aucune de nos poubelles correspondent)
		)t)
		

	)
)


; Moteur d'inférences
; -----------------------
(defun run ()
    (cond 
        ( (eval stopCondition) (print 'motor_stop) (print containerChosen ))
        ( 
            (null (findActivable)) (print 'problem_not_solve_or_nothing_applicable) nil 
        )
        ( t 
            (mapc 'myApply (findActivable)) ; on applique toutes les règles
            (run)
        )
    )
)

