import Itinerarios._
import ItinerariosPar._
import Datos._

val itineraryGenerator = itinerarios(vuelosCurso,aeropuertosCurso)
val itineraryGeneratorPar = itinerariosPar(vuelosCurso,aeropuertosCurso)

val caliMexico = itineraryGenerator("CLO", "MEX")
val caliMexicoPar = itineraryGeneratorPar("CLO", "MEX")
val caliMoscu = itineraryGenerator("CLO", "SVO")
val caliMoscuPar = itineraryGeneratorPar("CLO", "SVO")

itineraryGenerator("CLO", "SVCS")

tiempoTotalItinerario(aeropuertosCurso)(caliMexico.head)

for {
  vuelo <- caliMoscu
}yield tiempoTotalItinerario(aeropuertosCurso)(vuelo)

val itinerarioTiempo = itinerariosTiempo(vuelosCurso, aeropuertosCurso)
val itinerarioTiempoPar = itinerariosTiempoPar(vuelosCurso, aeropuertosCurso)

itinerarioTiempo("CLO", "SVO")
itinerarioTiempoPar("CLO", "SVO")


val itineraryUSA = itinerarios(vuelosC1,aeropuertos)
val itineraryUSApar = itinerariosPar(vuelosC1, aeropuertos)
val timeUSA = itinerariosTiempo(vuelosC1, aeropuertos)

val AtlLax = itineraryUSA("ATL", "LAX")
val AtlLaxPar = itineraryUSApar("ATL", "LAX")

AtlLax.equals(AtlLaxPar)


for {
  vuelo <- AtlLax
}yield tiempoTotalItinerario(aeropuertos)(vuelo)

val time = timeUSA("ATL", "LAX")

for {
  vuelo <- time
}yield tiempoTotalItinerario(aeropuertos)(vuelo)