import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

import scala.concurrent.duration.DurationInt

object Host {
  sealed trait HostCommand
  final case class AuctionItemWithThisClients(item: Item, clients: List[ActorRef[ClientCommand]]) extends HostCommand
  final case class ItemOffer(value: Int, whichClientOffered: ActorRef[ClientCommand]) extends HostCommand
  final case class Timeout() extends HostCommand
  final case class CloseAuction() extends HostCommand

  private var hostId: Int = 0
  private var hostState: HostState = new StartOfferHostState()
  private var clients: List[ActorRef[ClientCommand]] = List.empty
  private var currentOffer: Int = 0

  def apply(id: Int, room: ActorRef[RoomCommand]): Behavior[HostCommand] = {
    hostId = id
    waitingAuctionState(room)
  }

  def setState(state: HostState): Unit = {
    hostState = state
  }

  def waitingAuctionState(room: ActorRef[RoomCommand]): Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case AuctionItemWithThisClients(item, newClients) =>
          clients = newClients
          clients.foreach(_ ! StartingOfferOfItemAt(item, context.self))
          currentOffer = item.value
          auctioningState(item, room)
        case CloseAuction() =>
          // Este mensaje le llega del room cuando no hay mas items, y debe cerrarse el host
          Behaviors.stopped
        case _ =>
          Behaviors.ignore
      }
    }
  }

  def auctioningState(item: Item, room: ActorRef[RoomCommand]): Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case ItemOffer(newValue, whichClientOffered) =>
          println(f"[Host] New offer from $whichClientOffered")
          newValue match {
            case value if value <= currentOffer => setState(new WaitingOfferHostState(value, currentOffer))
            case _ => setState(new NewOfferHostState(context.self, newValue, clients))
          }
          execute()
          waitingOfferState(item, room)
        case Timeout() =>
          new FinishOfferHostState(room, clients).execute()
          waitingAuctionState(room)
        case _ =>
          Behaviors.ignore
      }
    }
  }

  def waitingOfferState(item: Item, room: ActorRef[RoomCommand]): Behavior[HostCommand] = {
    Behaviors.withTimers[HostCommand] { timers =>
      timers.startSingleTimer(f"host-$hostId", Timeout(), 7.seconds)
      auctioningState(item, room)
    }
  }

  def execute(): Unit = {
    hostState.execute()
  }
}
