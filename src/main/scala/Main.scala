import Owner.SendNextItem
import akka.NotUsed
import akka.actor.typed.{ActorSystem, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors

object Main {

  val itemsList: List[String] = List("Product1", "Product2", "Product3")

  def main(args: Array[String]): Unit = {
    ActorSystem(Main(), "ChatRoomDemo")
  }

  def apply(): Behavior[NotUsed] =
    Behaviors.setup { context =>
      val owner = context.spawn(Owner(itemsList, 3), "Owner")
      context.watch(owner)

      owner ! SendNextItem("sarasa")

      Behaviors.receiveSignal {
        case (_, Terminated(_)) =>
          Behaviors.stopped
      }
    }
}
