; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  CLASSES  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Clases definidas en la ontología (exportar de CLIPS)

; Thu Nov 29 12:33:09 GMT 2018
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
	(single-slot Nombre_Ejercicio
;+		(comment "Nombre del ejercicio")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Duracion
		(type INTEGER)
		(default 0)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Sesiones
		(type INSTANCE)
;+		(allowed-classes Session)
		(cardinality 3 7)
		(create-accessor read-write))
	(single-slot Nombre_Deporte
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Ejercicios
		(type INSTANCE)
;+		(allowed-classes Ejercicio)
		(create-accessor read-write))
	(single-slot Repeticiones_Ejercicio
		(type INTEGER)
		(default 1)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Repeticiones
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot IAPractica2_Class7
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Partes_Ejercitadas
		(type STRING)
		(create-accessor read-write))
	(single-slot Beneficios
		(type STRING)
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
	(multislot Partes_Ejercitadas
		(type STRING)
		(create-accessor read-write))
	(single-slot Repeticiones_Ejercicio
		(type INTEGER)
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
		(default 0)
;+		(cardinality 1 1)
		(create-accessor read-write)))

(defclass Aerobico "Aerobico o resistencia"
	(is-a Ejercicio)
	(role concrete))

(defclass Fuerza "Fuerza o musculación"
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

; Thu Nov 29 12:33:09 GMT 2018
;
;+ (version "3.5")
;+ (build "Build 663")

([IAPractica2_Class10] of  Aerobico

	(Duracion 30)
	(Intensidad 0)
	(Nombre_Ejercicio "Caminar")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class11] of  Aerobico

	(Duracion 30)
	(Intensidad 2)
	(Nombre_Ejercicio "Correr")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class12] of  Aerobico

	(Duracion 30)
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

	(Duracion 60)
	(Intensidad 1)
	(Nombre_Ejercicio "Caminar")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class16] of  Aerobico

	(Duracion 60)
	(Intensidad 2)
	(Nombre_Ejercicio "Marcha")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class17] of  Aerobico

	(Duracion 90)
	(Intensidad 2)
	(Nombre_Ejercicio "Caminar")
	(Partes_Ejercitadas "Piernas"))

([IAPractica2_Class19] of  Equilibrio

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Plantar")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class22] of  Equilibrio

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Rodilla")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class23] of  Equilibrio

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Flexion_Cadera")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class24] of  Equilibrio

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Levantar_Pierna")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class25] of  Flexibilidad

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Estiramiento_Muneca")
	(Partes_Ejercitadas "Brazos")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class26] of  Flexibilidad

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Rotacion_Hombro")
	(Partes_Ejercitadas
		"Torso"
		"Brazos")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class27] of  Flexibilidad

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "Estiramiento_Quadriceps")
	(Partes_Ejercitadas "Piernas")
	(Repeticiones_Ejercicio 3))

([IAPractica2_Class28] of  Fuerza

	(Duracion 2)
	(Intensidad 0)
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

([IAPractica2_Class30] of  Fuerza

	(Duracion 5)
	(Intensidad 2)
	(Nombre_Ejercicio "Flexiones")
	(Partes_Ejercitadas
		"Brazos"
		"Piernas")
	(Repeticiones_Ejercicio 5))


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
	(initial-fact)
	=>
	(printout t "SCB de rutinas" crlf)
	(printout t "por Sergi Aragall, Alberto Camacho y Albert Teira" crlf)
	(assert (newRutine))
	(focus question_module)
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
		
(deftemplate question_module::esta-en-rango "Si esta en rango puesto"
	(slot nombre (type STRING))
)

(deftemplate question_module::edad (slot numero (type INTEGER)))

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
	    (bind ?f (ask-question "Indique la frequencia con la que realiza ejercicio: \
					1- No realizo mas esfuerzos de los necesarios \
					2- Ocasionalmente realizo alguna actividad fisica \
					3- Regularmente realizo actividades fisicas" 1 2 3))
		)
		



(defrule question_module::edaddd
	(declare (salience 10))
	(newRutine)
	=>
    (if (yes-or-no-p "Diga su rango de edad? [65~80/>80]") then
     	(assert (esta-en-rango (nombre "SI")))
    else
		(assert (esta-en-rango (nombre "NO")))
	)
)

(defrule question_module::caidas
	(declare (salience 10))
	(newRutine)
	=>
    (if (yes-or-no-p "Ha sufrido alguna caída recientemente? [s/n]") then
		;action
    else
		;action
	)
)

(defrule question_module::problemas-movilidad
	(declare (salience 10))
	(newRutine)
	=>
    (if (yes-or-no-p "Sufre problemas de movilidad? [s/n]") then
		;action
    else
		;action
	)
)

;;; Añadir preguntas

; Para pasar al modulo de inferencia
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

(defrule inference_module::sacarPantalla
	(declare (salience 10))
	(conclusions)
  	?f<-(esta-en-rango (nombre ?nombre))
	=>
	(if (eq ?nombre "SI") then
		(printout t "Asi que estas dentro de la franja de edad eh, viejito lesbiano" crlf)
    else
		(printout t "O eres joven, adulto o una momia en vida, seas lo que seas esta app no es para ti chaval" crlf)
))



-------------------- FINISH MODULE --------------------------

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

