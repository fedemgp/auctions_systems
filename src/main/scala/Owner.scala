import Room.{AuctionItem, NoMoreItems, RoomCommand}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.NotUsed

object Owner {

  // private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

  sealed trait OwnerCommand
  final case class SendNextItem(replyTo: ActorRef[RoomCommand]) extends OwnerCommand

  def apply(items: List[String], rooms: Int): Behavior[OwnerCommand] = {
    var currentItems = items
    Behaviors.setup { context =>
      for (room <- 1 to rooms) {
        val roomMailBox = context.spawn(new RoomSession(room)(), "room" + room)
        val init :+ last = currentItems
        roomMailBox ! AuctionItem(last, context.self)
        currentItems = init
      }
      sendItem(currentItems, rooms)
    }
  }

  def sendItem(items: List[String], rooms: Int): Behavior[OwnerCommand] = {
    Behaviors.receive { (context, message) =>
      message match {
        case SendNextItem(replyTo) =>
          if (items.nonEmpty) {
            val remainderItems :+ last = items
            println("Comenzar a subastar item " + last)
            replyTo ! AuctionItem(last, context.self)
            sendItem(remainderItems, rooms)
          } else {
            println("Enviando fin de ejecución N° " + 1)
            replyTo ! NoMoreItems()
            endingProcess(1, rooms)
          }
      }
    }
  }

  def endingProcess(notifiedRooms: Int, rooms: Int): Behavior[OwnerCommand] = {
    Behaviors.receive { (context, message) =>
      message match {
        case SendNextItem(replyTo) =>
          println("Enviando fin de ejecución N° " + (notifiedRooms + 1))
          replyTo ! NoMoreItems()
          if ((notifiedRooms + 1) == rooms) {
            println("Fin del owner")
            Behaviors.stopped
          } else {
            endingProcess(notifiedRooms + 1, rooms)
          }
      }
    }
  }
}
