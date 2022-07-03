import Host.HostCommand
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

sealed trait ClientCommand

final case class StartingOfferOfItemAt(item: Item, replyTo: ActorRef[HostCommand]) extends ClientCommand
final case class ItemAt(newValue: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand
final case class AuctionEnded() extends ClientCommand
final case class MakeOffer(newOffer: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand

abstract class AbstractClient {
  var clientId: Int = 0
  var roomId: Int = 0

  def apply(clientId: Int, roomId: Int): Behavior[ClientCommand]

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
}
