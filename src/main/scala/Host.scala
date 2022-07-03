import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

import scala.concurrent.duration.DurationInt

object Host {
  sealed trait HostCommand
  final case class AuctionItemWithThisClients(item: Item, clients: List[ActorRef[ClientCommand]]) extends HostCommand
  final case class ItemOffer(value: Int, whichClientOffered: ActorRef[ClientCommand]) extends HostCommand
  final case class Timeout(value: Int, whichClientOffered: ActorRef[ClientCommand]) extends HostCommand
  final case class CloseAuction() extends HostCommand

  def apply(id: Int, room: ActorRef[RoomCommand]): Behavior[HostCommand] = {
    waitingAuctionState(id, room)
  }

  def waitingAuctionState(hostId: Int, room: ActorRef[RoomCommand]):
  Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case AuctionItemWithThisClients(item, newClients) =>
          println(Console.GREEN + f"[Host $hostId] Starting offer with initial value ${item.value}" + Console.RESET)
          newClients.foreach(_ ! StartingOfferOfItemAt(item, context.self))
          auctioningState(hostId, item, item.value, room, newClients)
        case CloseAuction() =>
          // Este mensaje le llega del room cuando no hay mas items, y debe cerrarse el host
          println(f"[Host $hostId] Closing auction")
          Behaviors.stopped
        case _ =>
          Behaviors.ignore
      }
    }
  }

  def auctioningState(hostId: Int, item: Item, oldValue: Int, room: ActorRef[RoomCommand], clients: List[ActorRef[ClientCommand]]):
  Behavior[HostCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case ItemOffer(newValue, whichClientOffered) =>
          println(f"[Host $hostId] New offer from $whichClientOffered")
          // If the bet is not higher ignore it
          if (newValue <= oldValue) {
            println(f"[Host $hostId] Invalid offer, $newValue is not higher than $oldValue")
            whichClientOffered ! ItemAt(oldValue, context.self)
            auctioningState(hostId, item, oldValue, room, clients)
          } else {
            println(f"[Host $hostId] New offer of $newValue")
            clients.foreach(_ ! ItemAt(newValue, context.self))
            waitingOfferState(hostId, item, newValue, room, clients, whichClientOffered)
          }
        case Timeout(value, whichClientOffered) =>
          println(f"[Host $hostId] We have a winner, $whichClientOffered, with an offer of $value!!")
          // Last send to close all clients
          clients.foreach(_ ! AuctionEnded())
          room ! FinishedSession()
          waitingAuctionState(hostId, room)
      }
    }
  }

  def waitingOfferState(hostId: Int, item: Item, newValue: Int, room: ActorRef[RoomCommand],
                        clients: List[ActorRef[ClientCommand]], lastOfferFrom: ActorRef[ClientCommand]):
  Behavior[HostCommand] = {
    Behaviors.withTimers[HostCommand] { timers =>
      timers.startSingleTimer(f"host-$hostId", Timeout(newValue, lastOfferFrom), 7.seconds)
      auctioningState(hostId, item, newValue, room, clients)
    }
  }
}
