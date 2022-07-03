import Host.{HostCommand, ItemOffer}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

import scala.concurrent.duration._
import scala.util.Random

object Client {
  sealed trait ClientCommand

  final case class StartingOfferOfItemAt(item: Item, replyTo: ActorRef[HostCommand]) extends ClientCommand
  final case class ItemAt(newValue: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand
  final case class AuctionEnded() extends ClientCommand
  final case class MakeOffer(newOffer: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand

  val rndEngine: Random = new Random(System.currentTimeMillis)
  var budget: Int = 0
  var clientId: Int = 0
  var roomId: Int = 0

  def apply(clientId: Int, roomId: Int): Behavior[ClientCommand] = {
    this.budget = rndEngine.between(100, 1000)
    println(f"[CLIENT $clientId, from room $roomId] starting client with budget $$$budget")
    this.clientId = clientId
    this.roomId = roomId
    offerLogic()
  }

  def offerLogic(): Behavior[ClientCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case StartingOfferOfItemAt(item, host) =>
          println(f"[CLIENT $clientId, from room: $roomId] new item ${item.name} arrived, with value ${item.value}")
          thinking(item.value, host)
        case ItemAt(value, host) =>
          println(f"[CLIENT $clientId, from room: $roomId] new item value is $value")
          thinking(value, host)
        case MakeOffer(newOffer, host) =>
          println(f"[CLIENT $clientId, from room: $roomId] Sending offer $newOffer to host ")
          host ! ItemOffer(newOffer, context.self)
          Behaviors.same
        case AuctionEnded() =>
          println(f"[CLIENT $clientId, from room: $roomId] auction ended")
          Behaviors.stopped
      }
    }
  }

  def surrender(): Behavior[ClientCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case AuctionEnded() =>
          println(f"[CLIENT $clientId, from room: $roomId] auction ended")
          Behaviors.stopped
        case _ =>
          Behaviors.ignore
      }
    }
  }

  def thinking(itemValue: Int, host: ActorRef[HostCommand]): Behavior[ClientCommand] = {
    Behaviors.withTimers[ClientCommand] { timers =>
      /*
       * Si el cliente no puede aumentar aunque sea una unidad la oferta, automaticamente se rinde de la subasta.
       * Con probabilidad de 0.3, puede rendirse aun pudiendo aumentar la oferta
       */
      if ((itemValue + 1 <= budget) && (rndEngine.nextFloat() > 0.3)) {
        val newOffer = itemValue +  rndEngine.between(1, budget - itemValue)
        val wait = rndEngine.between(5, 10)
        println(f"[CLIENT $clientId from room $roomId] waiting $wait seconds until make offer of $newOffer")
        /*
         * El primer argumento de startingSingleTimer es una clave, cuando se llama al método con la misma clave,
         * se reinicia el evento. Esto es util para evitar enviar una oferta de un cliente lento cuando la oferta
         * aumento mas de lo que pensaba aumentar inicialmente.
         */
        timers.startSingleTimer(f"$clientId-$roomId", MakeOffer(newOffer, host), wait.seconds)
        offerLogic()
      } else {
        println(f"[CLIENT $clientId, from room: $roomId] item is too expensive, i won't offer this")
        surrender()
      }
    }
  }
}
