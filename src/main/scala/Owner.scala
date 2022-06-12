import Room.AuctionItem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.NotUsed

object Owner {

  // private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

  sealed trait OwnerCommand
  final case class SendNextItem(replyTo: ActorRef[AuctionItem]) extends OwnerCommand

  def apply(items: List[String], rooms: Int): Behavior[OwnerCommand] = {
    var currentItems = items
    Behaviors.setup { context =>
      for (room <- 1 to rooms) {
        val roomMailBox = context.spawn(new RoomSession(room)(), "room" + room)
        val init :+ last = currentItems
        roomMailBox ! AuctionItem(last, context.self)
        currentItems = init
      }
      sendItem(currentItems)
    }
  }

  def sendItem(items: List[String]): Behavior[OwnerCommand] = {
    Behaviors.receive { (context, message) =>
      message match {
        case SendNextItem(replyTo) =>
          if (items.isEmpty) {
            return Behaviors.stopped
          } else {
            val remainderItems :+ last = items
            println("Comenzar a subastar item " + last)
            replyTo ! AuctionItem(last, context.self)
            return sendItem(remainderItems)
          }
      }
    }
  }
}
