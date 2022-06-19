import Client.{AuctionEnded, ClientCommand, ItemAt, StartingOfferOfItemAt}
import Owner.SendNextItem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object Host {
  sealed trait HostCommand
  final case class AuctionItemWithThisClients(item: Item, clients: List[ActorRef[ClientCommand]]) extends HostCommand
  final case class ItemOffer(value: Int, clients: List[ActorRef[ClientCommand]]) extends HostCommand
  final case class CloseAuction() extends HostCommand

  def apply(room: ActorRef[RoomCommand]): Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case AuctionItemWithThisClients(item, clients) =>
          clients.foreach(_ ! StartingOfferOfItemAt(item, context.self))
          Behaviors.same

        case ItemOffer(newValue, clients: List[ActorRef[ClientCommand]]) =>
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
          Behaviors.same

        case CloseAuction() =>
          Behaviors.stopped
      }
    }
  }
}
