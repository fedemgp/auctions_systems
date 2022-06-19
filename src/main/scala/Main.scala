import Owner.SendNextItem
import akka.NotUsed
import akka.actor.typed.{ActorSystem, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors

object Main {

  val itemsList: List[Item] = List(
    new Item("Product1", 100),
    new Item("Product2", 200),
    new Item("Product3", 10),
    new Item("Product4", 15))

  def main(args: Array[String]): Unit = {
    ActorSystem(Main(), "ChatRoomDemo")
  }

  def apply(): Behavior[NotUsed] =
    Behaviors.setup { context =>
      val owner = context.spawn(Owner(itemsList, 3), "Owner")
      context.watch(owner)

      Behaviors.receiveSignal {
        case (_, Terminated(_)) =>
          Behaviors.stopped
      }
    }
}
