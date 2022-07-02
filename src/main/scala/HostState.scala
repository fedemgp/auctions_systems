import Host.HostCommand
import akka.actor.typed.Behavior

trait HostState {

  def execute(): Behavior[HostCommand]
}
