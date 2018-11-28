; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  CLASSES  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Clases definidas en la ontología (exportar de CLIPS)

; Mon Nov 26 23:19:34 CET 2018
; 
;+ (version "3.5")
;+ (build "Build 663")


(defclass %3ACLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(multislot Sesiones
		(type INSTANCE)
;+		(allowed-classes Sesion)
		(cardinality 3 7)
		(create-accessor read-write))
	(single-slot nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Sesion
	(is-a USER)
	(role concrete))

(defclass Calentamiento
	(is-a USER)
	(role concrete)
	(single-slot nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Ejercicio
	(is-a USER)
	(role concrete))

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
	(role concrete))

(defclass ConjuntoSesiones
	(is-a USER)
	(role concrete)
	(multislot Sesiones
		(type INSTANCE)
;+		(allowed-classes Sesion)
		(cardinality 3 7)
		(create-accessor read-write)))
		
;DEFTEMPLATES:	

(deftemplate esta-en-rango "Si esta en rango puesto"
	(slot nombre (type STRING))
)


; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  INSTANCES  --------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Instancias (de CLIPS también)

; --------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------  FUNCTIONS  -------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Funciones generales

; --------------------------------------------------------------------------------------------------------------------
; ----------------------------------------------  TEMPLATES ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Yo crearía un template donde vayamos guardando la solución

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

; Función para comprobar que la respuesta que se entra sea valida
(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "Respuesta invalida. " ?question)
      (bind ?answer (read)))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer)))
   ?answer)

; Función para las preguntas de SI o NO
(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question si no s n))
   (if (or (eq ?response si) (eq ?response s))
       then TRUE 
       else FALSE))

(defrule question_module::edad
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


