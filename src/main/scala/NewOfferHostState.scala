import Client.{ClientCommand, ItemAt}
import akka.actor.typed.ActorRef

class NewOfferHostState(room: ActorRef[Host.HostCommand], currentValue: Int, clients: List[ActorRef[ClientCommand]]) extends HostState {

  override def execute(): Unit = {
    println(f"[Host] New offer of $currentValue")
    clients.foreach(_ ! ItemAt(currentValue, room))
  }
}

