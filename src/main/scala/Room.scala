import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object Room {

  sealed trait RoomCommand
  final case class AuctionItem(item: String, replyTo: ActorRef[Nothing]) extends RoomCommand

  def apply(): Behavior[RoomCommand] = Behaviors.receive { (context, message) => {
      message match {
        case AuctionItem(item, owner) =>
          println("Empecé a subastar " + item)
          Behaviors.same
      }
    }
  }
}
