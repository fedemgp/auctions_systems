import Client.{ClientCommand, ItemAt}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

class NewOfferHostState(room: ActorRef[Host.HostCommand], currentValue: Int, clients: List[ActorRef[ClientCommand]]) extends HostState {

  override def execute(): Behavior[Host.HostCommand] = {
    println(f"[Host] New offer of $currentValue")
    clients.foreach(_ ! ItemAt(currentValue, room))
    Behaviors.same
  }
}

