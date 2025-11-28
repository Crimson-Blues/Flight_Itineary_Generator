import Datos._
import common._
import Itinerarios._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

package object ItinerariosPar {

  def vuelosPosiblesPar(vuelosList: List[Vuelo])(vueloActual:Vuelo): List[Vuelo] = {
    val aeropuerto = vueloActual.Dst
    val vVuelos = vuelosList.toVector
    (for {
      vuelo <- vVuelos.par
      if vuelo != vueloActual
      if vuelo.Org == aeropuerto
    } yield vuelo).toList
  }

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String)=>List[Itinerario]={
    val proximosVuelos: Vuelo => List[Vuelo] = vuelosPosiblesPar(vuelos)

    def generadorItinerariosPar(org: String, dst: String): List[Itinerario] = {

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

      (for {
        vuelo <- vuelos.par
        if(vuelo.Org == org)
        itinerario <- recItinerariosPar(vuelo, Set(org))
      } yield itinerario).toList
    }
    generadorItinerariosPar
  }

  def mapaAeropuertosPar(aeropuertos: List[Aeropuerto]): Map[String, Aeropuerto] = {
    (for {
      aeropuerto <- aeropuertos.par
    } yield (aeropuerto.Cod, aeropuerto)).seq.toMap
  }

  def tiempoTotalItinerarioPar(aeropuertos: List[Aeropuerto])(itinerario: Itinerario):Int = {
    if(itinerario.isEmpty) 0
    else {
      val mapAero = mapaAeropuertosPar(aeropuertos)
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
    (org:String, dst:String) => {
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

      dividirItinerarios(listItinerarios).slice(0,3).map(_._1)
    }
  }

  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val calcEscalas: Itinerario => Int = escalasItinerario

    def organizarEscalas(org: String, dst: String): List[Itinerario] = {
      val listItinerarios = itinerariosPar(vuelos, aeropuertos)(org, dst)

      def dividirItinerarios(list: List[Itinerario]): List[(Itinerario, Int)] = {
        if (list.length < 10)
          list.map(it => (it, calcEscalas(it)))
        else {
          val m = list.length / 2
          val (l1, l2) = list.splitAt(m)

          val (res1, res2) = parallel(
            dividirItinerarios(l1),
            dividirItinerarios(l2)
          )
          (res1 ++ res2).sortBy(_._2)
        }
      }

      val ordenados = dividirItinerarios(listItinerarios)
      ordenados.slice(0, 3).map(_._1)
    }
    organizarEscalas
  }


  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):
  (String, String) => List[Itinerario] = {
    val tiempoVuelo: Itinerario => Int = tiempoVueloItinerario(aeropuertos)

    def organizarAire(org: String, dst: String): List[Itinerario] = {
      val listItinerarios = itinerariosPar(vuelos, aeropuertos)(org, dst)

      def dividirItinerarios(list: List[Itinerario]): List[(Itinerario, Int)] = {
        if (list.length < 10)
          list.map(it => (it, tiempoVuelo(it)))
        else {
          val m = list.length / 2
          val (l1, l2) = list.splitAt(m)

          val (res1, res2) = parallel(
            dividirItinerarios(l1),
            dividirItinerarios(l2)
          )

          (res1 ++ res2).sortBy(_._2)
        }
      }

      val ordenados = dividirItinerarios(listItinerarios)
      ordenados.slice(0, 3).map(_._1)
    }
    organizarAire
  }

  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    val mapaAero = mapaAeropuertos(aeropuertos)
    val calcTiempo: Itinerario => Int = tiempoTotalItinerario(aeropuertos)

    def itinerariosSalida(vuelos: Itinerario, horaCita: Int): Int = {
      val vueloFinal = vuelos.last
      val aeropuertoDestino = mapaAero(vueloFinal.Dst)
      val horaLlegada = tiempoUniversal(vueloFinal.HL, vueloFinal.ML, aeropuertoDestino.GMT)
      calcTiempo(vuelos) + arreglarTiempos(horaCita - horaLlegada)
    }

    def validarItinerariosDisponibles(vuelos: List[Itinerario], horaCita: Int)
    : List[(Itinerario, Int)] = {
      if (vuelos.length < 12)
        vuelos.map(itinerario => (itinerario, itinerariosSalida(itinerario, horaCita)))
      else {
        val (a, b) = vuelos.splitAt(vuelos.length / 2)
        val (pa, pb) = parallel(
          validarItinerariosDisponibles(a, horaCita),
          validarItinerariosDisponibles(b, horaCita)
        )
        pa ++ pb
      }
    }

    (codO: String, codD: String, h: Int, m: Int) => {
      val todos = itinerariosPar(vuelos, aeropuertos)(codO, codD)

      if (todos.isEmpty) Nil
      else {
        val horaCita = tiempoUniversal(h, m, mapaAero(codD).GMT)
        val itinerariosPosibles = validarItinerariosDisponibles(todos, horaCita)

        itinerariosPosibles.minBy { case (_, score) => score }._1
      }
    }
  }



}
