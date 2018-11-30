; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  CLASSES  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Clases definidas en la ontología (exportar de CLIPS)

; Fri Nov 30 09:52:54 GMT+01:00 2018
; 
;+ (version "3.5")
;+ (build "Build 663")


(defclass %3ACLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(multislot Dia
		(type INTEGER)
		(range 1 7)
		(create-accessor read-write))
	(single-slot Intensidad
		(type INTEGER)
		(range 0 2)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Repeticiones_Ejercicio
		(type INTEGER)
		(range 0 100)
		(default 1)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Duracion
		(type INTEGER)
		(range 0 180)
		(default 0)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Beneficios
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot IAPractica2_Class7
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Nombre_Deporte
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Ejercicios
		(type INSTANCE)
;+		(allowed-classes Ejercicio)
		(create-accessor read-write))
	(multislot Partes_Ejercitadas
		(type STRING)
		(create-accessor read-write))
	(multislot Sesiones
		(type INSTANCE)
;+		(allowed-classes Session)
		(cardinality 3 7)
		(create-accessor read-write))
	(single-slot Nombre_Ejercicio
;+		(comment "Nombre del ejercicio")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Repeticiones
		(type INTEGER)
		(range 0 10)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Session
	(is-a USER)
	(role concrete)
	(multislot Dia
		(type INTEGER)
		(range 1 7)
		(create-accessor read-write))
	(multislot Ejercicios
		(type INSTANCE)
;+		(allowed-classes Ejercicio)
		(create-accessor read-write)))

(defclass Ejercicio
	(is-a USER)
	(role concrete)
	(single-slot Intensidad
		(type INTEGER)
		(range 0 2)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Repeticiones_Ejercicio
		(type INTEGER)
		(range 0 100)
		(default 1)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Nombre_Ejercicio
;+		(comment "Nombre del ejercicio")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Duracion
		(type INTEGER)
		(range 0 180)
		(default 0)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Partes_Ejercitadas
		(type STRING)
		(create-accessor read-write)))

(defclass Aerobico "Aerobico o resistencia"
	(is-a Ejercicio)
	(role concrete))

(defclass Fuerza "Fuerza o musculacion"
	(is-a Ejercicio)
	(role concrete))

(defclass Equilibrio
	(is-a Ejercicio)
	(role concrete))

(defclass Flexibilidad
	(is-a Ejercicio)
	(role concrete))

(defclass Deporte
	(is-a USER)
	(role concrete)
	(single-slot Nombre_Deporte
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write)))


; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  INSTANCES  --------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Instancias (de CLIPS también)

(definstances Instancias

; Fri Nov 30 19:58:55 GMT+01:00 2018
; 
;+ (version "3.5")
;+ (build "Build 663")

([IAPractica2_Class0] of  Aerobico

	(Duracion 10)
	(Intensidad 0)
	(Nombre_Ejercicio "Marcha")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class10] of  Aerobico

	(Duracion 20)
	(Intensidad 0)
	(Nombre_Ejercicio "Caminar")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class10007] of  Flexibilidad

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Rotacion_Simple_Cadera")
	(Partes_Ejercitadas
		"Piernas"
		"Torso")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class10008] of  Aerobico

	(Duracion 30)
	(Intensidad 1)
	(Nombre_Ejercicio "Pedalear")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 1))

([IAPractica2_Class10009] of  Aerobico

	(Duracion 60)
	(Intensidad 2)
	(Nombre_Ejercicio "Pedalear")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 1))

([IAPractica2_Class10010] of  Fuerza

	(Duracion 5)
	(Intensidad 1)
	(Nombre_Ejercicio "Elevar_Piernas")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 10))

([IAPractica2_Class10011] of  Fuerza

	(Duracion 4)
	(Intensidad 1)
	(Nombre_Ejercicio "Extension_Cadera")
	(Partes_Ejercitadas
		"Piernas"
		"Torso")
	(Repeticiones_Ejercicio 4))

([IAPractica2_Class10013] of  Fuerza

	(Duracion 2)
	(Intensidad 1)
	(Nombre_Ejercicio "Flexion_Cadera")
	(Partes_Ejercitadas
		"Piernas"
		"Torso")
	(Repeticiones_Ejercicio 1))

([IAPractica2_Class10014] of  Fuerza

	(Duracion 4)
	(Intensidad 2)
	(Nombre_Ejercicio "Flexion_Cadera")
	(Partes_Ejercitadas
		"Piernas"
		"Torso")
	(Repeticiones_Ejercicio 2))

([IAPractica2_Class10015] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Extension_Rodilla")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 10))

([IAPractica2_Class10017] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Rodilla")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 10))

([IAPractica2_Class10018] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Plantar")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 10))

([IAPractica2_Class10019] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Levantarse_De_Silla")
	(Partes_Ejercitadas
		"Piernas"
		"Torso")
	(Repeticiones_Ejercicio 10))

([IAPractica2_Class10020] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Extension_Triceps")
	(Partes_Ejercitadas "Brazos")
	(Repeticiones_Ejercicio 10))

([IAPractica2_Class10021] of  Fuerza

	(Duracion 4)
	(Intensidad 1)
	(Nombre_Ejercicio "Extension_Triceps")
	(Partes_Ejercitadas "Brazos")
	(Repeticiones_Ejercicio 20))

([IAPractica2_Class10022] of  Fuerza

	(Duracion 6)
	(Intensidad 2)
	(Nombre_Ejercicio "Extension_Triceps")
	(Partes_Ejercitadas "Brazos")
	(Repeticiones_Ejercicio 30))

([IAPractica2_Class10023] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Hombro")
	(Partes_Ejercitadas
		"Brazos"
		"Torso")
	(Repeticiones_Ejercicio 10))

([IAPractica2_Class10024] of  Fuerza

	(Duracion 4)
	(Intensidad 1)
	(Nombre_Ejercicio "Flexion_Hombro")
	(Partes_Ejercitadas
		"Brazos"
		"Torso")
	(Repeticiones_Ejercicio 20))

([IAPractica2_Class10025] of  Fuerza

	(Duracion 6)
	(Intensidad 2)
	(Nombre_Ejercicio "Flexion_Hombro")
	(Partes_Ejercitadas
		"Brazos"
		"Torso")
	(Repeticiones_Ejercicio 30))

([IAPractica2_Class10026] of  Equilibrio

	(Duracion 5)
	(Intensidad 1)
	(Nombre_Ejercicio "Saltos_Torsion")
	(Partes_Ejercitadas
		"Torso"
		"Piernas"
		"Brazos")
	(Repeticiones_Ejercicio 20))

([IAPractica2_Class10027] of  Equilibrio

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Equilibrio_En_Barra")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 1))

([IAPractica2_Class10028] of  Equilibrio

	(Duracion 5)
	(Intensidad 2)
	(Nombre_Ejercicio "Pie_Talon")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 1))

([IAPractica2_Class10029] of  Equilibrio

	(Duracion 2)
	(Intensidad 2)
	(Nombre_Ejercicio "Plancha")
	(Partes_Ejercitadas
		"Torso"
		"Piernas"
		"Brazos")
	(Repeticiones_Ejercicio 1))

([IAPractica2_Class11] of  Aerobico

	(Duracion 30)
	(Intensidad 2)
	(Nombre_Ejercicio "Correr")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class12] of  Aerobico

	(Duracion 20)
	(Intensidad 1)
	(Nombre_Ejercicio "Marcha")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class13] of  Aerobico

	(Duracion 15)
	(Intensidad 1)
	(Nombre_Ejercicio "Subir_Escaleras")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class14] of  Aerobico

	(Duracion 30)
	(Intensidad 2)
	(Nombre_Ejercicio "Subir_Escaleras")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class15] of  Aerobico

	(Duracion 30)
	(Intensidad 1)
	(Nombre_Ejercicio "Caminar")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class16] of  Aerobico

	(Duracion 30)
	(Intensidad 2)
	(Nombre_Ejercicio "Marcha")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class17] of  Aerobico

	(Duracion 40)
	(Intensidad 2)
	(Nombre_Ejercicio "Caminar")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class19] of  Equilibrio

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Plantar")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class2] of  Flexibilidad

	(Duracion 10)
	(Intensidad 2)
	(Nombre_Ejercicio "Rotacion_Doble_Cadera")
	(Partes_Ejercitadas "Torso")
	(Repeticiones_Ejercicio 20))

([IAPractica2_Class22] of  Equilibrio

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Rodilla")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class23] of  Equilibrio

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Cadera")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class24] of  Equilibrio

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Levantar_Pierna")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class25] of  Flexibilidad

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Estiramiento_Muneca")
	(Partes_Ejercitadas "Brazos")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class26] of  Flexibilidad

	(Duracion 3)
	(Intensidad 0)
	(Nombre_Ejercicio "Rotacion_Hombro")
	(Partes_Ejercitadas
		"Torso"
		"Brazos")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class27] of  Flexibilidad

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Estiramiento_Quadriceps")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class28] of  Fuerza

	(Duracion 2)
	(Intensidad 1)
	(Nombre_Ejercicio "Levantar_Brazos")
	(Partes_Ejercitadas
		"Brazos"
		"Torso")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class29] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Extension_Cadera")
	(Partes_Ejercitadas
		"Piernas"
		"Torso")
	(Repeticiones_Ejercicio 2))

([IAPractica2_Class3] of  Flexibilidad

	(Duracion 5)
	(Intensidad 1)
	(Nombre_Ejercicio "Estiramiento_Tendones_Muslo")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class30] of  Fuerza

	(Duracion 5)
	(Intensidad 2)
	(Nombre_Ejercicio "Flexiones")
	(Partes_Ejercitadas
		"Brazos"
		"Piernas")
	(Repeticiones_Ejercicio 5))

([IAPractica2_Class4] of  Flexibilidad

	(Duracion 3)
	(Intensidad 0)
	(Nombre_Ejercicio "Estiramiento_Pantorrilla")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class5] of  Flexibilidad

	(Duracion 5)
	(Intensidad 0)
	(Nombre_Ejercicio "Estiramiento_Tobillo")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class6] of  Flexibilidad

	(Duracion 3)
	(Intensidad 0)
	(Nombre_Ejercicio "Estiramiento_Triceps")
	(Partes_Ejercitadas
		"Brazos"
		"Torso")
	(Repeticiones_Ejercicio 3))



)

; --------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------  FUNCTIONS  -------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Funciones generales


; --------------------------------------------------------------------------------------------------------------------
; ----------------------------------------------  TEMPLATES ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------


; --------------------------------------------------------------------------------------------------------------------
; --------------------------------------------------  MAIN  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------

(defmodule MAIN (export ?ALL))


	

(defrule MAIN::initial_rule "Regla inicial"
	?f <-(initial-fact)
	?ej <- (object (is-a Ejercicio)(Intensidad ?valor))
	=>
	(printout t "SCB de rutinas" crlf)
	(printout t "por Sergi Aragall, Alberto Camacho y Albert Teira" crlf)
	(assert (newRutine))
	(retract ?f) 
	(focus question_module)
)

(defrule MAIN::cal_reset "Estan carregades les instancies?"
	?f<-(initial-fact)
	=>
	(printout t "ERROR! falta cargar las instancias, haga (reset) antes de ejecutar el programa." crlf)
	(retract ?f)
)



; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  QUESTION MODULE  ----------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Definir preguntas para más adelante poder inferir


; Definimos el módulo para las preguntas
(defmodule question_module
	(import MAIN ?ALL)
	(export ?ALL)
)

;------------------------ FUNCIONES PREGUNTA --------------------

;pregunta y comprueba que el valor devuelto sea uno entre los puestos
(deffunction question_module::ask-question (?question $?allowed-values)
   (printout t ?question crlf)
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "ERROR, INTRODUZCA UN VALOR CORRECTO:  " ?question crlf)
      (bind ?answer (read)))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer)))
   ?answer)

;pregunta y comprueba si el resultado es si o no
(deffunction question_module::yes-or-no-p (?question)
   (bind ?response (ask-question ?question si no s n SI NO S N Y y yes YES))
   (if (or (eq ?response no) (eq ?response n) (eq ?response N) (eq ?response NO))
       then FALSE
       else TRUE))

;pregunta y comprueba que el valor devuelto este entre el rango
(deffunction question_module::pregunta-numerica (?pregunta ?rangini ?rangfi)
	(printout t ?pregunta "[" ?rangini "," ?rangfi "]" crlf)
	(bind ?respuesta (read))
	
	(while (or (not(numberp ?respuesta))(not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi)))) do		
		(printout t "ERROR, INTRODUZCA UN VALOR CONTENIDO EN EL INTERVALO: " ?pregunta "[" ?rangini "," ?rangfi "]" crlf)
		(bind ?respuesta (read))
	)
	?respuesta
)


;------------------------ FUNCIONES AUXILIARES --------------------


		
		
;-------------------- DEFTEMPLATE ----------------------
		
(deftemplate question_module::edad (slot numero (type INTEGER)))

(deftemplate question_module::capacidad (slot valor (type INTEGER) (range 1 3)))

(deftemplate question_module::colesterol (slot nivel (type INTEGER) (range 0 3)))


;------------ RULES ------------------------------

	;PREGUNTA EDAD
	(defrule question_module::pregunta_edad
		(declare (salience 10))
		(newRutine)
		=>
    	(bind ?f (pregunta-numerica "Indique cual es su edad" 0 100) )
		(assert (edad (numero ?f)))
		(if (< ?f 65) then
			(focus finish)
		))
		
	(defrule question_module::es_menor
		(declare (salience 10))
		(edad (numero ?f))
		=>
		(if (< ?f 65) then
			(pop-focus)
		))
		
		
		
	;REALIZA EJERCICIO
	(defrule question_module::realiza_ejercicio
		(declare (salience 10))
		(newRutine)
		=>
	    (bind ?f (ask-question "Indique la frecuencia con la que realiza ejercicio: \
					1- No realizo mas esfuerzos de los necesarios \
					2- Ocasionalmente realizo alguna actividad fisica \
					3- Regularmente realizo actividades fisicas" 1 2 3))
		(bind ?aux (- ?f 1))
		(assert (capacidad (valor ?aux)))
	)
	
	
	;ENFERMEDADES CARDIOVASCULARES
	(defrule question_module::question-enfermedad-cardiovascular
		(declare (salience 10))
		(newRutine)
		=>
		(if (yes-or-no-p "Sufre de alguna enfermedad cardiovascular? [S/N]") then
			(assert (enfermedad-cardiovascular))
		else
			(assert (colesterol (nivel 0)))
		)
	)

	(defrule question_module::question-colesterol
		(declare (salience 10))
		(newRutine)
		?g <- (enfermedad-cardiovascular)
		=>
		(bind ?f (ask-question "Como tiene el colesterol? \
				1- Normal \
				2- Elevado \
				3- Muy Alto" 1 2 3))
		(assert (colesterol (nivel ?f)))
		(retract ?g)
	)

		
	(defrule question_module::caidas
		(declare (salience 10))
		(newRutine)
		=>
		(if (yes-or-no-p "Ha sufrido alguna caida recientemente? [S/N]") then
			(assert (caida))
		)
	)




; PASAR MODULO INFERENCIA
(defrule question_module::end_questions
	(declare (salience 0))
	(newRutine)
	=>
	(assert (conclusions))
	(focus inference_module)
)

; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  INFERENCE MODULE  ---------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Módulo para hacer la inferencia de datos según las preguntas

; Definimos el módulo para la inferencia de datos
(defmodule inference_module
	(import MAIN ?ALL)
    (import question_module ?ALL)
    (export ?ALL)
)

(deffunction inference_module::programaSesion (?s $?allowed-values)
	(switch ?s (case 1 then (make-instance sesion1 of Session (Dia ?s) (Ejercicios $?allowed-values)))
			(case 2 then (make-instance sesion2 of Session (Dia ?s) (Ejercicios $?allowed-values)))
			(case 3 then (make-instance sesion3 of Session (Dia ?s) (Ejercicios $?allowed-values)))	
			(case 4 then (make-instance sesion4 of Session (Dia ?s) (Ejercicios $?allowed-values)))
			(case 5 then (make-instance sesion5 of Session (Dia ?s) (Ejercicios $?allowed-values)))
			(case 6 then (make-instance sesion6 of Session (Dia ?s) (Ejercicios $?allowed-values)))
			(case 7 then (make-instance sesion7 of Session (Dia ?s) (Ejercicios $?allowed-values)))
		)
)		

(deffunction inference_module::randomSlot ($?allowed-values)
	(bind ?tam (length $?allowed-values))
	(bind ?i (random 1 ?tam))
	(bind ?ejercicio (nth$ ?i $?allowed-values))
	?ejercicio
)
			
			
(deftemplate inference_module::sesion (slot num (type INTEGER)))

(deftemplate inference_module::imprimir_sesion (slot num (type INTEGER)))



(defrule inference_module::numeroSesiones
	(declare (salience 10))
	(conclusions)
	(capacidad (valor ?valor))
	=>
	(if (= ?valor 0) then (assert (sesion (num 5))) (assert (sesion (num 3))) (assert (sesion (num 1))))
	(if (= ?valor 1) then (assert (sesion (num 7))) (assert (sesion (num 5))) (assert (sesion (num 4))) (assert (sesion (num 3))) (assert (sesion (num 1))))
	(if (= ?valor 2) then (assert (sesion (num 7))) (assert (sesion (num 6))) (assert (sesion (num 5))) (assert (sesion (num 4))) (assert (sesion (num 3))) (assert (sesion (num 2))) (assert (sesion (num 1))))
	)
	
(defrule inference_module::programarSesion
	(declare (salience 10))
	(conclusions)
	?f<-(sesion (num ?s))
	(capacidad (valor ?valor))
	(colesterol (nivel ?nivel))
	=>
	(bind $?flex (find-all-instances ((?inst Flexibilidad)) (= ?inst:Intensidad ?valor)))
	(bind $?eq (find-all-instances ((?inst Equilibrio)) (= ?inst:Intensidad ?valor)))
	(bind $?fuer (find-all-instances ((?inst Fuerza)) (= ?inst:Intensidad ?valor)))
	
	(bind ?value 1)
	(if (= ?nivel 0) then (bind ?value ?valor))
	(if (= ?nivel 1) then (if (< ?valor 2) then (bind ?value ?valor)))
	(if (= ?nivel 2) then (bind ?value 0))
	
	(if (= ?nivel 3) then
		(programaSesion ?s (randomSlot $?flex) (randomSlot $?eq) (randomSlot $?fuer))	
	 else 
		(bind $?res (find-all-instances ((?inst Aerobico)) (= ?inst:Intensidad ?value)))
		(programaSesion ?s (randomSlot $?flex) (randomSlot $?eq) (randomSlot $?fuer) (randomSlot $?res))
	)
	
	(retract ?f)
	(assert (imprimir_sesion (num ?s)))
	)



(defrule inference_module::finalizarAnalisis
	(declare (salience 0))
	(conclusions)
	=>
	(assert (escribir))
	(focus output_module)	
)
	
	
	
	
; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  OUTPUT MODULE  ---------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
	
(defmodule output_module
    (import MAIN ?ALL)
    (import question_module ?ALL)
	(import inference_module ?ALL)
    (export ?ALL)
)

(deffunction output_module::imprimir_ejercicios (?sesion)
	(bind ?i 1)
	(while (<= ?i (length$ (send ?sesion get-Ejercicios)))
	do
		(bind ?ejercicio (nth$ ?i (send ?sesion get-Ejercicios)))
		(printout t " - Realizaremos el ejercicio " (send ?ejercicio get-Nombre_Ejercicio) " durante un tiempo de " (send ?ejercicio get-Duracion) " minutos." crlf)
		(bind ?i (+ ?i 1))
 )
)


(defrule output_module::general
		(declare (salience 10))
		(escribir)
		=>
		(printout t "Este es el diario de sesiones asociado a su diagnostico: " crlf)
)

(defrule output_module::comentarColesterol
		(declare (salience 10))
		(escribir)
		?f <- (colesterol (nivel ?lev))
		=>
		(if (= ?lev 1) then (printout t "Tenga precaucion con los ejercicios de resistencia y no lleve su cuerpo al maximo." crlf))
		(if (= ?lev 2) then (printout t "No se preocupe por realizar al completo los ejercicios de resistencia, tenga cuidado y no haga esfuerzos excesivos." crlf))
		(if (= ?lev 3) then (printout t "Evite los esfuerzos excesivos, no llegue a un nivel de cansancio elevado; nosotros nos hemos preocupado de recomendarle ejercicios aptos para usted." crlf))
		(retract ?f)
		)
		
(defrule output_module::sacarPantalla
	(declare (salience 10))
	(escribir)
	?f<-(imprimir_sesion (num ?n))
	?fc <- (object (is-a Session) (Dia ?n) (Ejercicios $?e))
	=>
	(printout t "EJERCICIOS PARA EL DIA " ?n ":" crlf)
	(imprimir_ejercicios ?fc)
	(retract ?f)
)
	
(defrule output_module::cuidadoCaidas
	(declare (salience 10))
	(escribir)
	?f<-(caida)
	=>
	(printout t "Se recomienda realizar los ejercicios con cuidado, focalizandose en los ejercicios de equilibrio. " crlf)
	(retract ?f)
	)
	
	
	
	
	
	
	

; -------------------- FINISH MODULE --------------------------

(defmodule finish
;(import MAIN ?ALL)
    (import question_module ?ALL)
    (export ?ALL)
)

(defrule finish::sacarFuera
	(declare (salience 10))
	=>
	(printout t "Vaya, parece que no esta dentro de la franja de edad necesaria para usar la aplicacion, \ 
				pero no tema, ya tendra tiempo para ser viejo; por ahora disfrute de los resquizos de juventud que le quedan" crlf)
	(pop-focus))