import Host.HostCommand
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object Client {
  sealed trait ClientCommand
  final case class StartingOfferOfItemAt(item: Item, replyTo: ActorRef[HostCommand]) extends ClientCommand
  final case class ItemAt(value: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand
  final case class AuctionEnded() extends ClientCommand

  def apply(id: Int): Behavior[ClientCommand] = Behaviors.receive {
    (context, message) => {
      case StartingOfferOfItemAt(item, intialValue) =>


    }
  }
}
