import Datos._
package object Itinerarios {
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]):(String, String)=>List[Itinerario]={
    def vuelosPosibles(vueloActual: Vuelo): List[Vuelo] = {
      val aeropuerto = vueloActual.Dst
      val horaLlegada = vueloActual.HL
      val minLlegada = vueloActual.ML
      for {
        vuelo <- vuelos
        horaSalida = vuelo.HS
        minSalida = vuelo.MS
        if(vuelo != vueloActual)
        if((vuelo.Org == aeropuerto)&(horaSalida >= horaLlegada)&(minSalida >= minLlegada))
      } yield vuelo
    }
    def f(org: String, dst: String) = {
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
            val proxVuelos = vuelosPosibles(vueloBase)
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

    f
  }
}
