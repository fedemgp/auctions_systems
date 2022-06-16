import Room.{AuctionItem, NoMoreItems, RoomCommand}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.NotUsed

object Owner {

  // private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

  sealed trait OwnerCommand
  final case class SendNextItem(replyTo: ActorRef[RoomCommand]) extends OwnerCommand

  def apply(items: List[String], desiredRooms: Int): Behavior[OwnerCommand] = {
    Behaviors.setup { context =>
      val (remainingItems, createdRooms) = spawnRooms(desiredRooms, items, context)
      if (remainingItems.isEmpty) {
        endingProcess(0, createdRooms)
      } else {
        sendItem(remainingItems, createdRooms)
      }
    }
  }

  def spawnRooms(rooms: Int, items: List[String], context: ActorContext[OwnerCommand]): (List[String], Int) = {
    /**
     * Lanza las salas que subastarán los items. Se lanzará el mínimo entre la cantidad de salas pedidas
     * y la cantidad de items a subastar (si items < salas, se crean len(items) salas ya que no se necesitan mas).
     *
     * Retorna los items que quedan para subastar
     */
    var currentItems = items
    var neededRooms = Math.min(rooms, items.length)
    for (room <- 1 to neededRooms) {
      val roomMailBox = context.spawn(new RoomSession(room)(), "room" + room)
      val init :+ last = currentItems
      roomMailBox ! AuctionItem(last, context.self)
      currentItems = init
    }
    (currentItems, neededRooms)
  }

  def sendItem(items: List[String], rooms: Int): Behavior[OwnerCommand] = {
    /**
     * Mientras queden items a subastar, este comportamiento escucha mensajes de tipo SendNextItem, enviandolé a las
     * salas que lo soliciten el siguiente item a subastar. Cuando se quede sin items a subastar, enviará
     * el primer mensaje NoMoreItems, y cambiará su comportamiendo para ir cerrando las salas.
     */
    Behaviors.receive { (context, message) =>
      message match {
        case SendNextItem(replyTo) =>
          if (items.nonEmpty) {
            val remainderItems :+ last = items
            replyTo ! AuctionItem(last, context.self)
            sendItem(remainderItems, rooms)
          } else {
            replyTo ! NoMoreItems()
            endingProcess(1, rooms)
          }
      }
    }
  }

  def endingProcess(notifiedRooms: Int, rooms: Int): Behavior[OwnerCommand] = {
    /**
     * Comportamiento encargado de avisarle a las distintas salas el fin de la subasta. Cuando le comunica a todos
     * que se subastaron todos los items, finaliza su propia ejecución.
     */
    if (notifiedRooms == rooms) {
      println("Fin del owner")
      return Behaviors.stopped
    }

    Behaviors.receive { (context, message) =>
      message match {
        case SendNextItem(replyTo) =>
          replyTo ! NoMoreItems()
          endingProcess(notifiedRooms + 1, rooms)
      }
    }
  }
}
