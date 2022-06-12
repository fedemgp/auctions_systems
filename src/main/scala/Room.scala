import Owner.SendNextItem
import Room.RoomCommand
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

class RoomSession(id: Int) {
  def apply(): Behavior[RoomCommand] = {
    Room(id)
  }
}

object Room {
  sealed trait RoomCommand
  final case class AuctionItem(item: String, replyTo: ActorRef[SendNextItem]) extends RoomCommand

  def apply(id: Int): Behavior[RoomCommand] = Behaviors.receive { (context, message) => {
    message match {
      case AuctionItem(item, owner) =>
        println("Room " + id + ": Subasté item  " + item)
        owner ! SendNextItem(context.self)
        Behaviors.same
    }
  }
  }
}