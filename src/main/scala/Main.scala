import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior, Terminated}

object Main extends App {

  val itemsList: List[Item] = List(
    new Item("Product1", 80),
    new Item("Product2", 50))

  val chatRoomDemo = ActorSystem(Main(), "ChatRoomDemo")

  def apply(): Behavior[NotUsed] =
    Behaviors.setup { context =>
      val owner = context.spawn(Owner(itemsList, 1), "Owner")
      context.watch(owner)

      Behaviors.receiveSignal {
        case (_, Terminated(_)) =>
          Behaviors.stopped
      }
    }
}
