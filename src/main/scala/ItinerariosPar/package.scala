import Datos._
import common._
import Itinerarios._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

package object ItinerariosPar {

  def vuelosPosiblesPar(vuelosList: List[Vuelo])(vueloActual:Vuelo): List[Vuelo] = {
    val aeropuerto = vueloActual.Dst
    val horaLlegada = vueloActual.HL
    val minLlegada = vueloActual.ML
    val vVuelos = vuelosList.toVector
    (for {
      vuelo <- vVuelos.par
      horaSalida = vuelo.HS
      minSalida = vuelo.MS
      llegaTotal = horaLlegada * 60 + minLlegada
      saleTotal  = horaSalida * 60 + minSalida
      if vuelo != vueloActual
      if vuelo.Org == aeropuerto
      //if saleTotal >= llegaTotal
    } yield vuelo).toList
  }

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String)=>List[Itinerario]={
    val proximosVuelos: Vuelo => List[Vuelo] = vuelosPosiblesPar(vuelos)

    def generadorItinerariosPar(org: String, dst: String): List[Itinerario] = {
      val vuelosIniciales = for {
        vuelo <- vuelos
        if(vuelo.Org == org)
      } yield vuelo

      def recItinerariosPar(vueloBase: Vuelo, visitados: Set[String]): List[Itinerario] = {
        val proxVuelos = proximosVuelos(vueloBase)
        proxVuelos.length match {
          //Si se llega al destino termina la recursión
          case _ if vueloBase.Dst == dst => List(List(vueloBase))
          //Si se pasa de nuevo por un aeropuerto ya visitado el itinerario no es válido
          case _ if visitados.contains(vueloBase.Dst) => List()
          //Caso secuencial para cantidades pequeñas de vuelos
          case n if n < 10 =>
            for {
              vuelo <- proxVuelos
              itinerario <- recItinerariosPar(vuelo, visitados + vueloBase.Dst)
            } yield vueloBase :: itinerario
          //Caso paralelo
          case n =>
            val m = proxVuelos.length / 2
            val (proxVuelos1, proxVuelos2) = proxVuelos.splitAt(m)

            val (itnros1, itnros2) = parallel({
              for {
                vuelo <- proxVuelos1
                itinerario <- recItinerariosPar(vuelo, visitados + vueloBase.Dst)
              } yield vueloBase :: itinerario
            },
              {
                for {
                  vuelo <- proxVuelos2
                  itinerario <- recItinerariosPar(vuelo, visitados + vueloBase.Dst)
                } yield vueloBase :: itinerario
              })

            itnros1 ++ itnros2
        }
      }

      val vuelosInit =
        if (vuelosIniciales.length >= 20) vuelosIniciales.par
        else vuelosIniciales
      (for{
        vueloInit <- vuelosInit
        itinerario <- recItinerariosPar(vueloInit, Set(org))
      } yield itinerario).toList
    }
    generadorItinerariosPar
  }

  def tiempoTotalItinerarioPar(aeropuertos: List[Aeropuerto])(itinerario: Itinerario):Int = {
    if(itinerario.isEmpty) 0
    else {
      val mapAero = mapaAeropuertos(aeropuertos)
      val precalc = itinerario.par.map { vuelo =>
        val aeroSalida = mapAero(vuelo.Org)
        val aeroLlegada = mapAero(vuelo.Dst)

        val tInicio = tiempoUniversal(vuelo.HS, vuelo.MS, aeroSalida.GMT)
        val tLlegada = tiempoUniversal(vuelo.HL, vuelo.ML, aeroLlegada.GMT)
        val duracion = arreglarTiempos(tLlegada - tInicio)

        (tInicio, tLlegada, duracion)
      }

      precalc.tail.foldLeft((precalc.head._2, precalc.head._3)) {
        case ((llegadaPrev, acc), (tInicio, tLlegada, duracion)) =>

          val tiempoEspera = arreglarTiempos(tInicio - llegadaPrev)
          (tLlegada, acc + duracion + tiempoEspera)
      }._2
    }
  }

  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String) => List[Itinerario] ={
    val calcTiempo: Itinerario => Int = tiempoTotalItinerarioPar(aeropuertos)
    def organizarTiempos(org:String, dst:String): List[Itinerario] = {
      val listItinerarios = itinerariosPar(vuelos, aeropuertos)(org,dst)

      def dividirItinerarios(listItinerarios:List[Itinerario]): List[(Itinerario, Int)] = {
        val listadoTiempos = if(listItinerarios.length < 10) listItinerarios.map(it => (it, calcTiempo(it)))
        else {
          val m = listItinerarios.length/2
          val (itnros1, itnros2) = listItinerarios.splitAt(m)
          val (ordn1, ordn2) = parallel(dividirItinerarios(itnros1),
            dividirItinerarios(itnros2))

          ordn1 ++ ordn2
        }
        listadoTiempos.sortBy(_._2)
      }

      dividirItinerarios(listItinerarios).slice(0,3).map(x => x._1)
    }

    organizarTiempos
  }

}
