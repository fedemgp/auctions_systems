import Client.{AuctionEnded, ClientCommand, ItemAt, StartingOfferOfItemAt}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object Host {
  sealed trait HostCommand

  final case class AuctionItemWithThisClients(item: Item, clients: List[ActorRef[ClientCommand]]) extends HostCommand

  final case class ItemOffer(value: Int, whichClientOffered: ActorRef[ClientCommand]) extends HostCommand

  final case class CloseAuction() extends HostCommand

  sealed trait AuctionCommand

  def apply(room: ActorRef[RoomCommand]): Behavior[HostCommand] =
    host(room, 0, List.empty)

  def host(room: ActorRef[RoomCommand], auctionsHosted: Int, clients: List[ActorRef[ClientCommand]]): Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case AuctionItemWithThisClients(item, newClients) =>
          newClients.foreach(_ ! StartingOfferOfItemAt(item, context.self))
          // podria spawnearse en un child actor con otros behaviours, pero bueno XD
          val nextAuctionNumber = auctionsHosted + 1
          host(room, nextAuctionNumber, newClients)
        case ItemOffer(newValue, whichClientOffered) =>
          // TODO: agregar corte por tiemout
          if (newValue > 50) {
            // TODO: notificarle quien ganó?
            println("Tenemos un ganador!")
            // Last send to close all clients
            clients.foreach(_ ! AuctionEnded())
            room ! FinishedSession()
          } else {
            clients.foreach(_ ! ItemAt(newValue, context.self))
          }
          host(room, auctionsHosted, clients)
        case CloseAuction() =>
          room ! FinishedSession()
          // si hacemos stop aca no hay mas host, o no?
          Behaviors.same
      }
    }
  }
}
