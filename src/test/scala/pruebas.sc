import Itinerarios._
import Datos._

val itineraryGenerator = itinerarios(vuelosCurso,aeropuertosCurso)

val caliMexico = itineraryGenerator("CLO", "MEX")
val caliMoscu = itineraryGenerator("CLO", "SVO")

itineraryGenerator("CLO", "SVCS")

tiempoTotalItinerario(aeropuertosCurso)(caliMexico.head)

for {
  vuelo <- caliMoscu
}yield tiempoTotalItinerario(aeropuertosCurso)(vuelo)

val itinerarioTiempo = itinerariosTiempo(vuelosCurso, aeropuertosCurso)

itinerarioTiempo("CLO", "SVO")
