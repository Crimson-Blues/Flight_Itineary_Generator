import Datos._
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
      if saleTotal >= llegaTotal
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
  def tiempoVueloItinerario(aeropuertos: List[Aeropuerto])(itinerario: Itinerario): Int = {
    val mapAero = mapaAeropuertos(aeropuertos)
    (for {
      vuelo <- itinerario
      aeropuertoSalida = mapAero(vuelo.Org)
      aeropuertoLlegada = mapAero(vuelo.Dst)
      tiempoSalida = vuelo.HS*60 + vuelo.MS - (aeropuertoSalida.GMT/100)*60
      tiempoLlegada = vuelo.HL*60 + vuelo.ML - (aeropuertoLlegada.GMT/100)*60
    } yield (tiempoLlegada - tiempoSalida)).fold(0)((x:Int,y:Int)=> x+y)
  }

  def tiempoTotalItinerario(aeropuertos: List[Aeropuerto])(itinerario: Itinerario):Int = {
    if(itinerario.isEmpty) 0
    else {
      val mapAero = mapaAeropuertos(aeropuertos)
      val vItinerario = itinerario.toVector
      val primerVuelo = vItinerario(0)
      val ultimoVuelo = vItinerario(vItinerario.length - 1)
      val aeroOrigen = mapAero(primerVuelo.Org)
      val aeroDestino = mapAero(ultimoVuelo.Dst)
      val tiempoInicio = primerVuelo.HS*60 + primerVuelo.MS - (aeroOrigen.GMT/100)*60
      val tiempoFinal = ultimoVuelo.HL*60 + ultimoVuelo.ML - (aeroDestino.GMT/100)*60

      tiempoFinal - tiempoInicio
    }

  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String) => List[Itinerario] ={
    val calcTiempo: Itinerario => Int = tiempoTotalItinerario(aeropuertos)
    (org:String, dst:String) => {
      val listItinerarios = itinerarios(vuelos, aeropuertos)(org,dst)
      val ordenados = listItinerarios.sortBy(calcTiempo)

      ordenados.slice(0,4)
    }
  }
}
