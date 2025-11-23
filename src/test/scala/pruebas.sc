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
val itsEscalasCurso= itinerariosEscalas(vuelosCurso,aeropuertosCurso)
val itsc1 = itsEscalasCurso("MID", "SVCS")
val itsc2 = itsEscalasCurso("CLO", "SVCS")
val itsc3 = itsEscalasCurso("CLO", "SVO")
val itsc4 = itsEscalasCurso("CLO", "MEX")
val itsc5 = itsEscalasCurso("CTG", "PTY")

val itsEscalasCursoPar= itinerariosEscalasPar(vuelosCurso,aeropuertosCurso)
val itscPar1 = itsEscalasCurso("MID", "SVCS")
val itscPar2 = itsEscalasCurso("CLO", "SVCS")
val itscPar3 = itsEscalasCurso("CLO", "SVO")
val itscPar4 = itsEscalasCurso("CLO", "MEX")
val itscPar5 = itsEscalasCurso("CTG", "PTY")

val itsAireCurso = itinerariosAire(vuelosCurso, aeropuertosCurso)
val itsa1 = itsAireCurso("MID", "SVCS")
val itsa2 = itsAireCurso("CLO", "SVCS")
val itsa3 = itsAireCurso("CLO", "SVO")
val itsa4 = itsAireCurso("CLO", "MEX")
val itsa5 = itsAireCurso("CTG", "PTY")

val itsAireParCurso = itinerariosAirePar(vuelosCurso, aeropuertosCurso)
val itsaPar1 = itsAireParCurso("MID", "SVCS")
val itsaPar2 = itsAireParCurso("CLO", "SVCS")
val itsaPar3 = itsAireParCurso("CLO", "SVO")
val itsaPar4 = itsAireParCurso("CLO", "MEX")
val itsaPar5 = itsAireParCurso("CTG", "PTY")

