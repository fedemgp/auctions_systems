import Owner.{OwnerCommand, SendNextItem}
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
  final case class AuctionItem(item: String, replyTo: ActorRef[OwnerCommand]) extends RoomCommand
  final case class NoMoreItems() extends RoomCommand

  def apply(id: Int): Behavior[RoomCommand] = Behaviors.receive { (context, message) => {
    message match {
      case AuctionItem(item, owner) =>
        println("Room " + id + ": Subasté item  " + item)
        owner ! SendNextItem(context.self)
        Behaviors.same
      case NoMoreItems() =>
        println("Room " + id + ": Finalizo ejecución")
        Behaviors.stopped
    }
  }}
}