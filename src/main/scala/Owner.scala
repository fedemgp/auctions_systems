import Room.AuctionItem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.NotUsed

object Owner {

  // private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

  sealed trait OwnerCommand
  final case class SendNextItem(item: String) extends OwnerCommand

  def apply(items: List[String], rooms: Int): Behavior[OwnerCommand] = {
    var currentItems = items
    Behaviors.setup { context =>
      for (room <- 1 to rooms) {
        val roomMailBox = context.spawn(Room(), "room" + room)
        val init :+ last = currentItems
        roomMailBox ! AuctionItem(last, context.self)
        currentItems = init
      }
      Behaviors.receive { (context, message) =>
        message match {
          case SendNextItem(item) =>
            println("Empecé a subastar " + item)
            Behaviors.stopped
        }
      }
    }
  }
}
