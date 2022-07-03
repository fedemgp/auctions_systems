import Host.HostCommand
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

sealed trait ClientCommand

final case class StartingOfferOfItemAt(item: Item, replyTo: ActorRef[HostCommand]) extends ClientCommand
final case class ItemAt(newValue: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand
final case class AuctionEnded() extends ClientCommand
final case class MakeOffer(newOffer: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand

abstract class AbstractClient(clientId: Int, roomId: Int, isBot: Boolean) {
  def apply(): Behavior[ClientCommand]

  def surrender(): Behavior[ClientCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case AuctionEnded() =>
          if (isBot) {
            println(f"[Bot $clientId] auction ended")
          } else {
            println("[User] auction ended")
          }
          Behaviors.stopped
        case _ =>
          Behaviors.same
      }
    }
  }
}
