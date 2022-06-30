import Client.{AuctionEnded, ClientCommand, ItemAt, StartingOfferOfItemAt}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object Host {
  sealed trait HostCommand
  final case class AuctionItemWithThisClients(item: Item, clients: List[ActorRef[ClientCommand]]) extends HostCommand
  final case class ItemOffer(value: Int, whichClientOffered: ActorRef[ClientCommand]) extends HostCommand
  final case class CloseAuction() extends HostCommand

  def apply(room: ActorRef[RoomCommand]): Behavior[HostCommand] =
    host(room, 0, List.empty)

  def host(room: ActorRef[RoomCommand], auctionsHosted: Int, clients: List[ActorRef[ClientCommand]]): Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case AuctionItemWithThisClients(item, newClients) =>
          newClients.foreach(_ ! StartingOfferOfItemAt(item, context.self))
          // podria spawnearse en un child actor con otros behaviours, pero bueno XD
          val nextAuctionNumber = auctionsHosted + 1
          auctionWithItemAndValue(item, item.value, auctionsHosted, room, newClients)
        case CloseAuction() =>
          // Este mensaje le llega del room cuando no hay mas items, y debe cerrarse el host
          Behaviors.stopped
        case _ =>
          println("Ignoring message")
          Behaviors.same
      }
    }
  }
  // TODO: separarlo en dos chld actors distintos, uno que hable con el room y otro con el cliente
  // TODO 2: este metodo debe tener una referencia al que hizo la oferta anterior para compararlo con withClientOffered
  //         e ignorar dos aumentos consecutivos del mismo
  def auctionWithItemAndValue(item: Item, oldValue: Int, auctionsHosted: Int, room: ActorRef[RoomCommand],
                              clients: List[ActorRef[ClientCommand]]): Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case ItemOffer(newValue, whichClientOffered) =>
          // If the bet is not higher ignore it
          if (newValue <= oldValue) {
            println(f"[Host] Repeated offer with value $newValue (old value $oldValue)")
            auctionWithItemAndValue(item, oldValue, auctionsHosted, room, clients)
          } else {
            // TODO: agregar corte por tiemout
            if (newValue > 90) {
              // TODO: notificarle quien ganó?
              println("[Host] We have a winner!")
              // Last send to close all clients
              clients.foreach(_ ! AuctionEnded())
              // posible race condition?
              room ! FinishedSession()
              host(room, auctionsHosted, clients)
            } else {
              println(f"[Host] New offer of $newValue")
              clients.foreach(_ ! ItemAt(newValue, context.self))
              auctionWithItemAndValue(item, newValue, auctionsHosted, room, clients)
            }
          }
      }
    }
  }
}
