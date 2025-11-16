import Datos._

import scala.annotation.tailrec
package object Itinerarios {

  def vuelosPosibles(vuelosList: List[Vuelo])(vueloActual:Vuelo): List[Vuelo] = {
    val aeropuerto = vueloActual.Dst
    val horaLlegada = vueloActual.HL
    val minLlegada = vueloActual.ML
    for {
      vuelo <- vuelosList
      horaSalida = vuelo.HS
      minSalida = vuelo.MS
      llegaTotal = horaLlegada * 60 + minLlegada
      saleTotal  = horaSalida * 60 + minSalida
      if vuelo != vueloActual
      if vuelo.Org == aeropuerto
      //if saleTotal >= llegaTotal
    } yield vuelo
  }

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String)=>List[Itinerario]={
    val proximosVuelos: Vuelo => List[Vuelo] = vuelosPosibles(vuelos)

    def generadorItinerarios(org: String, dst: String) = {
      val vuelosIniciales = for {
        vuelo <- vuelos
        if(vuelo.Org == org)
      } yield vuelo

      def recItinerarios(vueloBase: Vuelo, visitados: Set[String]): List[Itinerario] = {
        //Si ya se llegó al destino termina recursión
        if (vueloBase.Dst == dst) List(List(vueloBase))
        else {
          //Si se retorna a un aeropuerto anterior se termina la recursión
          if(visitados.contains(vueloBase.Dst)) List()
          else {
            val proxVuelos = proximosVuelos(vueloBase)
            for {
              vuelo <- proxVuelos
              itinerario <- recItinerarios(vuelo, visitados + vueloBase.Dst)
            } yield vueloBase :: itinerario
          }
        }
      }
      for{
        vueloInit <- vuelosIniciales
        itinerario <- recItinerarios(vueloInit, Set(org))
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
      tiempoLlegada = tiempoUniversal(vuelo.HL, vuelo.ML, aeropuertoSalida.GMT)
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
}
