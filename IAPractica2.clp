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

(defrule MAIN::initialRule "Regla inicial"
	(initial-fact)
	=>
	(printout t "SCB de rutinas" crlf)
	(printout t "por Sergi Aragall, Alberto Camacho y Albert Teira" crlf)
	(assert (newRutine))
	(focus questionModule)
)

; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  QUESTION MODULE  ----------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Definir preguntas para más adelante poder inferir

; Definimos el módulo para las preguntas
(defmodule questionModule
	(import MAIN ?ALL)
	(export ?ALL)
)

; Función para comprobar que la respuesta que se entra sea valida
(deffunction questionModule::pregunta-opciones (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   ; lexemp -> string/symbol, integerp -> integer
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "Respuesta no valida. " ?question crlf)
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer
)

; Función para las preguntas de SI o NO
(deffunction questionModule::pregunta-si-no (?question)
   (bind ?response (pregunta-opciones ?question s n)))
   (if (eq ?response s)
       then TRUE
   else FALSE)
)

(defrule questionModule::regla1 ""
	(declare (salience 10))
	(newRutine)
	=>
    (if (pregunta-si-no "Le duele la cabeza? [s/n] ") then
	   (printout t "Si" crlf)
    else
		(printout t "No" crlf)
	)
)

; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  INFERENCE MODULE  ---------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Módulo para hacer la inferencia de datos según las preguntas

; Definimos el módulo para la inferencia de datos
(defmodule inferenceModule
	(import MAIN ?ALL)
    (import modulo_preguntas ?ALL)
    (export ?ALL)
)