import Itinerarios._
import ItinerariosPar._
import Datos._
import org.scalameter._


//---------Pruebas itinerarios-------------
val itSmall = itinerarios(vuelosCurso,aeropuertosCurso)
val it100 = itinerarios(vuelosC1,aeropuertos)
val it200 = itinerarios(vuelosC1 ++ vuelosC2, aeropuertos)
//val it300 = itinerarios(vuelosC1 ++ vuelosC2 ++ vuelosC3, aeropuertos)
//val it500 = itinerarios(vuelosD1, aeropuertos)
//val it1000 = itinerarios(vuelosD1 ++ vuelosD2, aeropuertos)
//val itMax = itinerarios(vuelos, aeropuertos)

val caliMexico = itSmall("CLO", "MEX")
val nonValid = itSmall("SO", "SA")
val caliMoscu = itSmall("CLO", "SVO")


//--------itinerariosPar---------------
val itSmallPar = itinerariosPar(vuelosCurso,aeropuertosCurso)
val it100Par = itinerariosPar(vuelosC1,aeropuertos)
val it200Par = itinerariosPar(vuelosC1 ++ vuelosC2, aeropuertos)
//val it500Par = itinerariosPar(vuelosD1, aeropuertos)
//val it1000Par = itinerariosPar(vuelosD1 ++ vuelosD2, aeropuertos)
//val itMaxPar = itinerariosPar(vuelos, aeropuertos)

val caliMexicoPar = itSmallPar("CLO", "MEX")
val nonValidPar = itSmallPar("SO", "SA")
val caliMoscuPar = itSmallPar("CLO", "SVO")


//---------itinerariosTiempo-------------
val itSmallTiempo = itinerariosTiempo(vuelosCurso,aeropuertosCurso)
val it100Tiempo = itinerariosTiempo(vuelosC1,aeropuertos)
val it200Tiempo = itinerariosTiempo(vuelosC1 ++ vuelosC2, aeropuertos)
//val it300 = itinerarios(vuelosC1 ++ vuelosC2 ++ vuelosC3, aeropuertos)
//val it500 = itinerarios(vuelosD1, aeropuertos)
//val it1000 = itinerarios(vuelosD1 ++ vuelosD2, aeropuertos)
//val itMax = itinerarios(vuelos, aeropuertos)

val caliMexico = itSmallTiempo("CLO", "MEX")
val nonValid = itSmallTiempo("SO", "SA")
val caliMoscu = itSmallTiempo("CLO", "SVO")


//--------itinerariosTiempoPar---------------
val itSmallTiempoPar = itinerariosTiempoPar(vuelosCurso,aeropuertosCurso)
val it100TiempoPar = itinerariosTiempoPar(vuelosC1,aeropuertos)
val it200TiempoPar = itinerariosTiempoPar(vuelosC1 ++ vuelosC2, aeropuertos)
//val it500Par = itinerariosPar(vuelosD1, aeropuertos)
//val it1000Par = itinerariosPar(vuelosD1 ++ vuelosD2, aeropuertos)
//val itMaxPar = itinerariosPar(vuelos, aeropuertos)

val caliMexicoPar = itSmallTiempoPar("CLO", "MEX")
val nonValidPar = itSmallTiempoPar("SO", "SA")
val caliMoscuPar = itSmallTiempoPar("CLO", "SVO")

//-------------itinerariosEscalas-------------

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

//----------itinerariosAire--------------
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

//----------itinerariosSalida--------------
val itSalidaCurso = itinerarioSalida(vuelosCurso,aeropuertosCurso)
val itSal1 = itSalidaCurso("CTG","PTY",11,40)
val itSal2 = itSalidaCurso("CTG","PTY",11,55)
val itSal3 = itSalidaCurso("CTG","SMR",10,30)
val itSal4 = itSalidaCurso("CTG","PTY",10,30)
val itSal5 = itSalidaCurso("BOG","BOG",11,30)

val itSalidaCursoPar = itinerarioSalidaPar(vuelosCurso, aeropuertosCurso)
val itSal11 = itSalidaCursoPar("CTG","PTY",9,40)
val itSal12 = itSalidaCursoPar("CTG","PTY",11,55)
val itSal13 = itSalidaCursoPar("CTG","SMR",9,30)
val itSal14 = itSalidaCursoPar("CTG", "PTY", 10, 30)
val itSal15 = itSalidaCursoPar("YTR", "PTY", 14, 20)


//---------Recopilación de pruebas de rendimiento--------

//-----------Función para probar tiempos de ejecucción------
import java.io.{PrintStream}

def flush(): Unit = {
  System.out.flush()
}

def tiempoDe[T](body: => T) = {
  val timeA1 = config(
    KeyValue(Key.exec.minWarmupRuns -> 1),
    KeyValue(Key.exec.maxWarmupRuns -> 2),
    KeyValue(Key.verbose -> false)
  ) withWarmer(new Warmer.Default) measure (body)
  timeA1
}

case class Row(nombre: String, sec: Double, par: Double, accel: Double)
case class Algoritmo(
                      nombre: String,
                      f: (List[Vuelo], List[Aeropuerto]) => (String, String) => List[Itinerario]
                    )

def imprimirTabla(titulo: String, rows: List[Row]): Unit = {
  println(s"\n=== $titulo ===")

  val header = f"${"Caso"}%-12s | ${"Secuencial"}%-12s | ${"Paralelo"}%-12s | ${"Aceleración"}%-12s"
  println(header)
  println("-" * header.length)

  rows.foreach { r =>
    println(f"${r.nombre}%-12s | ${r.sec}%-12.4f | ${r.par}%-12.4f | ${r.accel}%-12.4f")
  }
}


val viajes = List(("ATL", "LAX"), ("DCA", "SFO"), ("MIA", "DFW"))

val funciones: List[(Algoritmo, Algoritmo)] =
  List(
    (Algoritmo("itinerarios", itinerarios), Algoritmo("itinerariosPar", itinerariosPar)),
    (Algoritmo("itinerariosTiempo", itinerariosTiempo), Algoritmo("itinerariosTiempoPar", itinerariosTiempoPar)),
    (Algoritmo("itinerariosAire", itinerariosAire), Algoritmo("itinerariosAirePar", itinerariosAirePar)),
    (Algoritmo("itinerariosEscalas", itinerariosEscalas), Algoritmo("itinerariosEscalasPar", itinerariosEscalasPar))
  )

for {alg <- funciones
     viaje <- viajes} {
  val fnSec = alg._1.f
  val fnPar = alg._2.f
  val name = alg._1.nombre

  val secSmall = fnSec(vuelosCurso,aeropuertosCurso)
  val sec40 = fnSec(vuelosB1,aeropuertos)
  val sec100 = fnSec(vuelosC1,aeropuertos)
  val sec140 = fnSec(vuelosC1 ++ vuelosB1,aeropuertos)
  val sec200 = fnSec(vuelosC1 ++ vuelosC2, aeropuertos)

  val parSmall = fnPar(vuelosCurso,aeropuertosCurso)
  val par40 = fnPar(vuelosB1,aeropuertos)
  val par100 = fnPar(vuelosC1,aeropuertos)
  val par140 = fnPar(vuelosC1 ++ vuelosB1,aeropuertos)
  val par200 = fnPar(vuelosC1 ++ vuelosC2, aeropuertos)

  val secSmallTime = tiempoDe(secSmall("CLO", "SVO"))
  val sec40Time = tiempoDe(sec40(viaje._1, viaje._2))
  val sec100Time = tiempoDe(sec100(viaje._1, viaje._2))
  val sec140Time = tiempoDe(sec140(viaje._1, viaje._2))
  //val sec200Time = tiempoDe(sec200(viaje._1, viaje._2))

  val parSmallTime = tiempoDe(parSmall("CLO", "SVO"))
  val par40Time = tiempoDe(par40(viaje._1, viaje._2))
  val par100Time = tiempoDe(par100(viaje._1, viaje._2))
  val par140Time = tiempoDe(par140(viaje._1, viaje._2))
  //val par200Time = tiempoDe(par200(viaje._1, viaje._2))

  val accelSmall = secSmallTime.value/parSmallTime.value
  val accel40 = sec40Time.value/par40Time.value
  val accel100 = sec100Time.value/par100Time.value
  val accel140 = sec140Time.value/par140Time.value
  //val accel200 = sec200Time.value/par200Time.value

  val rows = List(
    Row("Curso", secSmallTime.value, parSmallTime.value, accelSmall),
    Row("40 vuelos", sec40Time.value, par40Time.value, accel40),
    Row("100 vuelos", sec100Time.value, par100Time.value, accel100),
    Row("140 vuelos", sec140Time.value, par140Time.value, accel140),
    //Row("200 vuelos", sec200Time.value, par200Time.value, accel200)
  )

  imprimirTabla(s"Algoritmo: $name | Viaje: ${viaje._1} -> ${viaje._2}", rows)
  flush()
}
