import Client.{AuctionEnded, ClientCommand, ItemAt, StartingOfferOfItemAt}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object Host {
  sealed trait HostCommand
  final case class AuctionItemWithThisClients(item: Item, clients: List[ActorRef[ClientCommand]]) extends HostCommand
  final case class ItemOffer(value: Int, whichClientOffered: ActorRef[ClientCommand]) extends HostCommand
  final case class CloseAuction() extends HostCommand

  private var hostState: HostState = new StartOfferHostState()

  def apply(room: ActorRef[RoomCommand]): Behavior[HostCommand] =
    host(room, 0, List.empty)

  def setState(state: HostState): Unit = {
    hostState = state
  }

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
          newValue match {
            case value if value <= oldValue => setState(new WaitingOfferHostState(value, oldValue))
            case value if value > 90 => setState(new FinishOfferHostState(room, clients))
            case _ => setState(new NewOfferHostState(context.self, newValue, clients))
          }
          execute()
      }
    }
  }

  def execute(): Behavior[HostCommand] = {
    hostState.execute()
  }
}
