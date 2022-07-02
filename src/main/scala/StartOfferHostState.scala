import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
class StartOfferHostState extends HostState {

  override def execute(): Behavior[Host.HostCommand] = {
    Behaviors.same
  }
}
