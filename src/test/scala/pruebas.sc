import Itinerarios._
import Datos._

val itineraryGenerator = itinerarios(vuelosCurso,aeropuertosCurso)

val caliMexico = itineraryGenerator("CLO", "MEX")
itineraryGenerator("CLO", "SVCS")

tiempoTotalItinerario(aeropuertosCurso)(caliMexico.tail.head)

val itinerarioTiempo = itinerariosTiempo(vuelosCurso, aeropuertosCurso)

itinerarioTiempo("CLO", "SVO")