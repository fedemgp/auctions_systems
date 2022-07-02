import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

class WaitingOfferHostState(currentValue: Int, oldValue: Int) extends HostState {

  override def execute(): Behavior[Host.HostCommand] = {
    println(f"[Host] Repeated offer with value $currentValue (old value $oldValue)")
    Behaviors.same
  }
}
