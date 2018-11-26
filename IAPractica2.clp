; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  CLASSES  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Clases definidas en la ontología (exportar de CLIPS)

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
		//action
    else
		//action
	)
)

(defrule question_module::caidas
	(declare (salience 10))
	(newRutine)
	=>
    (if (yes-or-no-p "Ha sufrido alguna caída recientemente? [s/n]") then
		//action
    else
		//action
	)
)

(defrule question_module::problemas-movilidad
	(declare (salience 10))
	(newRutine)
	=>
    (if (yes-or-no-p "Sufre problemas de movilidad? [s/n]") then
		//action
    else
		//action
	)
)

;;; Añadir preguntas

; Para pasar al modulo de inferencia
(defrule question_module::end_questions
	(declare (salience 0))
	(newRutine)
	=>
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
