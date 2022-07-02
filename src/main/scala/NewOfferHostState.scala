import Client.{ClientCommand, ItemAt}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

class NewOfferHostState(currentValue: Int, clients: List[ActorRef[ClientCommand]]) extends HostState {

  override def execute(): Behavior[Host.HostCommand] = Behaviors.setup { context =>
    println(f"[Host] New offer of $currentValue")
    clients.foreach(_ ! ItemAt(currentValue, context.self))
    Behaviors.same
  }
}

