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
      if llegaTotal >= saleTotal
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
}
