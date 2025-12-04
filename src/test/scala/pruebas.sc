import Itinerarios._
import ItinerariosPar._
import Datos._
import org.scalameter._


//---------Pruebas itinerarios-------------

//----------itinerarios------------
val itsCurso= itinerarios(vuelosCurso,aeropuertosCurso)
val its1 = itsCurso("MID", "SVCS")
val its2 = itsCurso("CLO", "SVCS")
val its3 = itsCurso("CLO", "SVO")
val its4 = itsCurso("CLO", "MEX")
val its5 = itsCurso("CTG", "PTY")

val itsCursoPar= itinerariosPar(vuelosCurso,aeropuertosCurso)
val itsPar1 = itsCursoPar("MID", "SVCS")
val itsPar2 = itsCursoPar("CLO", "SVCS")
val itsPar3 = itsCursoPar("CLO", "SVO")
val itsPar4 = itsCursoPar("CLO", "MEX")
val itsPar5 = itsCursoPar("CTG", "PTY")


//---------itinerariosTiempo-------------
val itsTCurso= itinerariosTiempo(vuelosCurso,aeropuertosCurso)
val itsT1 = itsTCurso("MID", "SVCS")
val itsT2 = itsTCurso("CLO", "SVCS")
val itsT3 = itsTCurso("CLO", "SVO")
val itsT4 = itsTCurso("CLO", "MEX")
val itsT5 = itsTCurso("CTG", "PTY")

val itsTCursoPar= itinerariosTiempoPar(vuelosCurso,aeropuertosCurso)
val itsTPar1 = itsTCursoPar("MID", "SVCS")
val itsTPar2 = itsTCursoPar("CLO", "SVCS")
val itsTPar3 = itsTCursoPar("CLO", "SVO")
val itsTPar4 = itsTCursoPar("CLO", "MEX")
val itsTPar5 = itsTCursoPar("CTG", "PTY")



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
    KeyValue(Key.exec.minWarmupRuns -> 5),
    KeyValue(Key.exec.maxWarmupRuns -> 10),
    KeyValue(Key.verbose -> false)
  ) withWarmer(new Warmer.Default) measure (body)
  timeA1
}

case class Row(nombre: String, sec: Double, par: Double, accel: Double, numIt: Int, maxIt: Int, averageIt: Double)
case class Algoritmo(
                      nombre: String,
                      f: (List[Vuelo], List[Aeropuerto]) => (String, String) => List[Itinerario]
                    )

def imprimirTabla(titulo: String, rows: List[Row]): Unit = {
  println(s"\n=== $titulo ===")

  val header = f"${"Caso"}%-12s | ${"Secuencial"}%-12s | ${"Paralelo"}%-12s | ${"Aceleración"}%-12s | ${"No.Itnros"}%-12s | ${"Max It"}%-12s | ${"It. Promedio"}%-12s"
  println(header)
  println("-" * header.length)

  rows.foreach { r =>
    println(f"${r.nombre}%-12s | ${r.sec}%-12.4f | ${r.par}%-12.4f | ${r.accel}%-12.4f | ${r.numIt}%-12s | ${r.maxIt}%-12s | ${r.averageIt}%-12.4f")
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

  var listSizeSmall = 0
  var listSize40 = 0
  var listSize100 = 0
  var listSize140 = 0
  var listSize200 = 0

  var maxSmall = 0
  var max40 = 0
  var max100 = 0
  var max140 = 0
  var max200 = 0

  var avSmall = 0.0
  var av40 = 0.0
  var av100 = 0.0
  var av140 = 0.0
  var av200 = 0.0


  val secSmallTime = tiempoDe(secSmall("CLO", "SVO"))
  val sec40Time = tiempoDe(sec40(viaje._1, viaje._2))
  val sec100Time = tiempoDe(sec100(viaje._1, viaje._2))
  val sec140Time = tiempoDe(sec140(viaje._1, viaje._2))
  //val sec200Time = tiempoDe(sec200(viaje._1, viaje._2))

  val parSmallTime = tiempoDe({
    val itSmall = parSmall("CLO", "SVO")
    listSizeSmall = itSmall.length
    maxSmall = itSmall.maxBy(_.length).length
    avSmall = itSmall.map(x => x.length).sum.toDouble/itSmall.length
  })
  val par40Time = tiempoDe({
    val it40 = par40(viaje._1, viaje._2)
    listSize40 = it40.length
    max40 = it40.maxBy(_.length).length
    av40 = it40.map(x => x.length).sum.toDouble/it40.length
  })
  val par100Time = tiempoDe({
    val it100 = par100(viaje._1, viaje._2)
    listSize100 = it100.length
    max100 = it100.maxBy(_.length).length
    av100 = it100.map(x => x.length).sum.toDouble/it100.length
  })
  val par140Time = tiempoDe({
    val it140 = par140(viaje._1, viaje._2)
    listSize140 = it140.length
    max140 = it140.maxBy(_.length).length
    av140 = it140.map(x => x.length).sum.toDouble/it140.length
  })
//  val par200Time = tiempoDe({
//    val it200 = par200(viaje._1, viaje._2)
//    listSize200 = it200.length
//    max200 = it200.maxBy(_.length).length
//    av200 = it200.map(x => x.length).sum.toDouble/it200.length
//  })

  val accelSmall = secSmallTime.value/parSmallTime.value
  val accel40 = sec40Time.value/par40Time.value
  val accel100 = sec100Time.value/par100Time.value
  val accel140 = sec140Time.value/par140Time.value
  //val accel200 = sec200Time.value/par200Time.value

  val rows = List(
    Row("Curso", secSmallTime.value, parSmallTime.value, accelSmall, listSizeSmall, maxSmall, avSmall),
    Row("40 vuelos", sec40Time.value, par40Time.value, accel40, listSize40, max40, av40),
    Row("100 vuelos", sec100Time.value, par100Time.value, accel100, listSize100, max100, av100),
    Row("140 vuelos", sec140Time.value, par140Time.value, accel140, listSize140, max140, av140),
    //Row("200 vuelos", sec200Time.value, par200Time.value, accel200, listSize200, max200, av200)
  )

  imprimirTabla(s"Algoritmo: $name | Viaje: ${viaje._1} -> ${viaje._2}", rows)
  flush()
}


//--------------Para itinerarioSalida----------
case class AlgoritmoS(
                      nombre: String,
                      f: (List[Vuelo], List[Aeropuerto]) => (String, String, Int, Int) => Itinerario
                    )
val funcion: (AlgoritmoS, AlgoritmoS) =
    (AlgoritmoS("itinerarioSalida", itinerarioSalida), AlgoritmoS("itinerarioSalidaPar", itinerarioSalidaPar))
for {
     viaje <- viajes} {
  val fnSec = funcion._1.f
  val fnPar = funcion._2.f
  val name = funcion._1.nombre

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

  var listSizeSmall = 0
  var listSize40 = 0
  var listSize100 = 0
  var listSize140 = 0
  var listSize200 = 0

  var maxSmall = 0
  var max40 = 0
  var max100 = 0
  var max140 = 0
  var max200 = 0

  var avSmall = 0.0
  var av40 = 0.0
  var av100 = 0.0
  var av140 = 0.0
  var av200 = 0.0

  val secSmallTime = tiempoDe(secSmall("CLO", "SVO", 12, 0))
  val sec40Time = tiempoDe(sec40(viaje._1, viaje._2, 12 ,0))
  val sec100Time = tiempoDe(sec100(viaje._1, viaje._2, 12, 0))
  val sec140Time = tiempoDe(sec140(viaje._1, viaje._2, 12 , 0))
  val sec200Time = tiempoDe(sec200(viaje._1, viaje._2, 12, 0))

  val parSmallTime = tiempoDe(parSmall("CLO", "SVO", 12, 0))
  val par40Time = tiempoDe(par40(viaje._1, viaje._2, 12, 0))
  val par100Time = tiempoDe(par100(viaje._1, viaje._2, 12, 0))
  val par140Time = tiempoDe(par140(viaje._1, viaje._2, 12, 0))
  val par200Time = tiempoDe(par200(viaje._1, viaje._2, 12, 0))

  val accelSmall = secSmallTime.value/parSmallTime.value
  val accel40 = sec40Time.value/par40Time.value
  val accel100 = sec100Time.value/par100Time.value
  val accel140 = sec140Time.value/par140Time.value
  val accel200 = sec200Time.value/par200Time.value

  val rows = List(
    Row("Curso", secSmallTime.value, parSmallTime.value, accelSmall, listSizeSmall, maxSmall, avSmall),
    Row("40 vuelos", sec40Time.value, par40Time.value, accel40, listSize40, max40, av40),
    Row("100 vuelos", sec100Time.value, par100Time.value, accel100, listSize100, max100, av100),
    Row("140 vuelos", sec140Time.value, par140Time.value, accel140, listSize140, max140, av140),
    Row("200 vuelos", sec200Time.value, par200Time.value, accel200, listSize200, max200, av200)
  )

  imprimirTabla(s"Algoritmo: $name | Viaje: ${viaje._1} -> ${viaje._2}", rows)
  flush()
}
