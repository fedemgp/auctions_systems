import Client.{AuctionEnded, ClientCommand}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

class FinishOfferHostState(room: ActorRef[RoomCommand], clients: List[ActorRef[ClientCommand]]) extends HostState {

  override def execute(): Behavior[Host.HostCommand] = {
    // TODO: notificarle quien ganó?
    println("[Host] We have a winner!")
    // Last send to close all clients
    clients.foreach(_ ! AuctionEnded())
    // posible race condition?
    room ! FinishedSession()
    Behaviors.same
  }
}
