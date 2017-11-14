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

( setq green_container (make-container
	:color "green"
	:max_capacity "100"
   	:waste_amount "0"
   	:authorized_material_list ("glass")
   	:unauthorized_dirt("chimic liquid","grease")
   	)
)

( setq yellow_container (make-container
	:color "yellow"
	:max_capacity "100"
   	:waste_amount "0"
   	:authorized_material_list ("carton","metal","plastic")
   	:unauthorized_dirt("chimic liquid","grease")
   	)
)

( setq blue_container (make-container
	:color "blue"
	:max_capacity "100"
   	:waste_amount "0"
   	:authorized_material_list ("paper")
   	:unauthorized_dirt()
   	)
)

( setq waste (make-waste 
	:material "Plastic"
   	:volume "1"
   	:dirt_type nil 
   	)
) 

;stop condition
(setq stopCondition
	(cond
		
		((or (>= green_container.max_capacity green_container.waste_amount)
			(>= blue_container.max_capacity blue_container.waste_amount)
			(>= yellow_container.max_capacity yellow_container.waste_amount)
		)t)
		

	)
)