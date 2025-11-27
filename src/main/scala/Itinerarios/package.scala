import Datos._

import scala.annotation.tailrec
package object Itinerarios {

  def vuelosPosibles(vuelosList: List[Vuelo])(vueloActual:Vuelo): List[Vuelo] = {
    val aeropuerto = vueloActual.Dst
    for {
      vuelo <- vuelosList
      if vuelo != vueloActual
      if vuelo.Org == aeropuerto
    } yield vuelo
  }

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String)=>List[Itinerario]={
    val proximosVuelos: Vuelo => List[Vuelo] = vuelosPosibles(vuelos)

    def generadorItinerarios(org: String, dst: String) = {

      def recItinerarios(vueloBase: Vuelo, visitados: Set[String]): List[Itinerario] = {
        val proxVuelos = proximosVuelos(vueloBase)
        proxVuelos.length match {
          //Si ya se llegó al destino termina recursión
          case _ if (vueloBase.Dst == dst) => List(List(vueloBase))
          case _ if(visitados.contains(vueloBase.Dst)) => List()
          case _ =>
            for {
              vuelo <- proxVuelos
              itinerario <- recItinerarios(vuelo, visitados + vueloBase.Dst)
            } yield vueloBase :: itinerario
        }
      }
      for {
        vuelo <- vuelos
        if(vuelo.Org == org)
        itinerario <- recItinerarios(vuelo, Set(org))
      } yield itinerario
    }

    generadorItinerarios
  }

  def mapaAeropuertos(aeropuertos: List[Aeropuerto]): Map[String, Aeropuerto] = {
    (for {
      aeropuerto <- aeropuertos
    } yield (aeropuerto.Cod, aeropuerto)).toMap
  }

  @tailrec
  def arreglarTiempos(tiempo:Int): Int = {
    if(tiempo > 0) tiempo
    else arreglarTiempos(tiempo + 24*60)
  }

  def tiempoUniversal(horas:Int, minutos:Int, gmt:Int):Int = {
    horas*60 + minutos - (gmt/100)*60
  }

  def tiempoVueloItinerario(aeropuertos: List[Aeropuerto])(itinerario: Itinerario): Int = {
    val mapAero = mapaAeropuertos(aeropuertos)
    (for {
      vuelo <- itinerario
      aeropuertoSalida = mapAero(vuelo.Org)
      aeropuertoLlegada = mapAero(vuelo.Dst)
      tiempoSalida = tiempoUniversal(vuelo.HS, vuelo.MS, aeropuertoSalida.GMT)
      tiempoLlegada = tiempoUniversal(vuelo.HL, vuelo.ML, aeropuertoLlegada.GMT)
    } yield (arreglarTiempos(tiempoLlegada - tiempoSalida))).sum
  }

  def tiempoTotalItinerario(aeropuertos: List[Aeropuerto])(itinerario: Itinerario):Int = {
    if(itinerario.isEmpty) 0
    else {
      val mapAero = mapaAeropuertos(aeropuertos)
      val primerVuelo = itinerario.head
      val aero1Salida = mapAero(primerVuelo.Org)
      val aero1Llegada = mapAero(primerVuelo.Dst)
      val t1Inicio = tiempoUniversal(primerVuelo.HS, primerVuelo.MS, aero1Salida.GMT)
      val t1Llegada = tiempoUniversal(primerVuelo.HL, primerVuelo.ML, aero1Llegada.GMT)

      val primeraDuracion = arreglarTiempos(t1Llegada - t1Inicio)

      itinerario.tail.foldLeft((t1Llegada, primeraDuracion)) {
        case ((llegadaPrev, acc), vuelo) =>
          val aeroSalida = mapAero(vuelo.Org)
          val aeroLlegada = mapAero(vuelo.Dst)

          val tInicio = tiempoUniversal(vuelo.HS, vuelo.MS, aeroSalida.GMT)
          val tLlegada = tiempoUniversal(vuelo.HL, vuelo.ML, aeroLlegada.GMT)

          val tiempoVuelo = arreglarTiempos(tLlegada - tInicio)
          val tiempoEspera = arreglarTiempos(tInicio - llegadaPrev)

          (tLlegada, acc + tiempoVuelo + tiempoEspera)
      }._2
    }
  }

    def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String) => List[Itinerario] ={
    val calcTiempo: Itinerario => Int = tiempoTotalItinerario(aeropuertos)
    (org:String, dst:String) => {
      val listItinerarios = itinerarios(vuelos, aeropuertos)(org,dst)
      val ordenados = listItinerarios.sortBy(calcTiempo)

      ordenados.slice(0,3)
    }
  }

  def escalasItinerario(it: Itinerario): Int = if (it.isEmpty) 0 else (it.length - 1) + it.map(_.Esc).sum

//  def primerosTres[A](lista: List[A]): List[A] = lista match {
//    case a :: b :: c :: _ => List(a, b, c)
//    case a :: b :: Nil    => List(a, b)
//    case a :: Nil         => List(a)
//    case Nil              => Nil
//  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val base = itinerarios(vuelos, aeropuertos)

    (org: String, dst: String) => {
      val ordenados = base(org, dst).sortBy(escalasItinerario)
      ordenados.slice(0, 3)
    }
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val tiempoVuelo: Itinerario => Int = tiempoVueloItinerario(aeropuertos)

    (org: String, dst: String) => {
      val itins = itinerarios(vuelos, aeropuertos)(org, dst)
      val ordenados = itins.sortBy(tiempoVuelo)
      ordenados.slice(0, 3)
    }
  }


  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):
  (String, String, Int, Int) => Itinerario = {

    val mapaAero = mapaAeropuertos(aeropuertos)
    def hora(h: Int, m: Int, gmt: Int): Int = tiempoUniversal(h, m, gmt)
    def cita(dst: String, h: Int, m: Int): Int = {
      val gmt = mapaAero(dst).GMT
      hora(h, m, gmt)
    }

    (cod1: String, cod2: String, h: Int, m: Int) => {

      val itinerariosDisponibles = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val horaCita = cita(cod2, h, m)

      val itinerariosValidos =
        itinerariosDisponibles.filter { it =>
          val ultimoVuelo = it.last
          val aeropuertaLlegada = mapaAero(ultimoVuelo.Dst)

          val descensoDestino = hora(ultimoVuelo.HL, ultimoVuelo.ML, aeropuertaLlegada.GMT)

          descensoDestino <= horaCita
        }

      if (itinerariosValidos.isEmpty) List()
      else {

        itinerariosValidos.maxBy { it =>
          val primerVuelo = it.head
          val aeropuertoSalida = mapaAero(primerVuelo.Org)

          hora(primerVuelo.HS, primerVuelo.MS, aeropuertoSalida.GMT)
        }
      }
    }
  }

}
