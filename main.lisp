;fact base


(defstruct waste
  	material
  	volume
  	dirt_type
)

(defstruct selectiveTrash 
  	container_list
    
)

(defstruct container
	color 
   	max_capacity
   	waste_amount
   	authorized_material_list
   	unauthorized_dirt_list
)

(setq green_container (make-container
	:color "green"
	:max_capacity "100"
   	:waste_amount "0"
   	:authorized_material_list ("glass")
   	:unauthorized_dirt("chimic liquid" "grease")
   	)
)

(setq yellow_container (make-container
	:color "yellow"
	:max_capacity "100"
   	:waste_amount "0"
   	:authorized_material_list ("carton" "metal" "plastic")
   	:unauthorized_dirt("chimic liquid" "grease")
   	)
)

(setq blue_container (make-container
	:color "blue"
	:max_capacity "100"
   	:waste_amount "0"
   	:authorized_material_list ("paper")
   	:unauthorized_dirt()
   	)
)

(setq waste (make-waste 
	:material "Paper"
   	:volume "1"
   	:dirt_type "clean" 
   	)
)   


;Rule list
(defvar LR '())

; Définition des structures
(defstruct rule name condition action weight)
(defstruct fact name attributes is_a)

; Fonctions de manipulations
(defun newrule(myname mycondition myaction myweight)
    (push (make-rule ; comme le new en POO
               :name myname
               :condition mycondition
               :action myaction
               :weight myweight
          )
    LR)
)

; Tester si une règle est applicable 
(defun applicable (x)
    (eval (get x 'condition ) )) ; on évalue la condition  retourn bool

;Savoir si une règle a déjà été appliquée
(defun appliquee (x)
    (get x 'appliquee) ) ; récupère la propriété appliquée

; findActivable
(defun findActivable ()
        (setq rulesList nil)
        (mapc 
            (lambda (x) 
                (cond
                    (and (not (null (appliquee x))) applicable x)
                    (setq rulesList (cons x rulesList))
                )
            )
        LR)
        rulesList
)
; adding rules

; r1 
;; condition -> si waste.material == glass 
;; action -> null
;; weight 1
(newrule 'r1 '(eq  (waste-material waste) "Paper") '(print ""))

; r2
;; condition -> si waste.dirt_type == clean && r1.appliquee==true
;; action -> mettre dans la glassTrash
;; weight 13
(newrule 'r2 '(eq  (and (eq (waste-dirt_type waste) "clean" ) r1 appliquee)) '(setq containerChoosen "blue") )



; CA -> trahs are full or no waste to add

(setq CA t)

;stop condition
(setq stopCondition
	(cond
		
		((or (>= (container-max_capacity green_container) (container-waste_amount green_container))
			(>= (container-max_capacity blue_container) (container-waste_amount blue_container))
			(>= (container-max_capacity yellow_container) (container-waste_amount yellow_container))
            (not (null containerChoosen))  ;on s'arrete quand la poubelle choisi est défini
                                            ;poubelle peut etre blue, yellow, green...ou other(aucune de nos poubelles correspondent)
		)t)
		

	)
)


; Moteur d'inférences
; -----------------------
(defun run ()
    (cond 
        ( (eval stopCondition) (print 'motor_stop) t )
        ( 
            (null (conflits)) (print 'problem_not_solve_or_nothing_applicable) nil  
        )
        ( t 
            (mapc 'appliquer (conflictset)) ; on applique toutes les règles
            (run)
        )
    )
)