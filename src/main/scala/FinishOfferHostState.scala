import Client.{AuctionEnded, ClientCommand}
import akka.actor.typed.{ActorRef}

class FinishOfferHostState(room: ActorRef[RoomCommand], clients: List[ActorRef[ClientCommand]]) extends HostState {

  override def execute(): Unit = {
    // TODO: notificarle quien ganó?
    println("[Host] We have a winner!")
    // Last send to close all clients
    clients.foreach(_ ! AuctionEnded())
    // posible race condition?
    room ! FinishedSession()
  }
}
