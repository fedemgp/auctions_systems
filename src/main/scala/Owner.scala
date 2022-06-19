import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

object Owner {

  // private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

  sealed trait OwnerCommand
  final case class SendNextItem(replyTo: ActorRef[RoomCommand]) extends OwnerCommand

  def apply(items: List[Item], desiredRooms: Int): Behavior[OwnerCommand] = {
    Behaviors.setup { context =>
      val (remainingItems, createdRooms) = spawnRooms(desiredRooms, items, context)
      if (remainingItems.isEmpty) {
        endingProcess(0, createdRooms)
      } else {
        sendItem(remainingItems, createdRooms)
      }
    }
  }

  def spawnRooms(rooms: Int, items: List[Item], context: ActorContext[OwnerCommand]): (List[Item], Int) = {
    /**
     * Lanza las salas que subastarán los items. Se lanzará el mínimo entre la cantidad de salas pedidas
     * y la cantidad de items a subastar (si items < salas, se crean len(items) salas ya que no se necesitan mas).
     *
     * Retorna los items que quedan para subastar
     */
    var currentItems = items
    val neededRooms = Math.min(rooms, items.length)
    for (room <- 1 to neededRooms) {
      val roomMailBox = context.spawn(new RoomSession(room, context.self).start(), "room" + room)
      val init :+ last = currentItems
      roomMailBox ! AuctionItem(last)
      currentItems = init
    }
    (currentItems, neededRooms)
  }

  def sendItem(items: List[Item], rooms: Int): Behavior[OwnerCommand] = {
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
            replyTo ! AuctionItem(last)
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
