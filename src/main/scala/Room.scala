import Host.AuctionItemWithThisClients
import Owner.{OwnerCommand, SendNextItem}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import com.sun.security.ntlm.Client
import _root_.Client.ClientCommand

sealed trait RoomCommand
final case class AuctionItem(item: Item) extends RoomCommand
final case class NoMoreItems() extends RoomCommand
final case class FinishedSession() extends RoomCommand

class RoomSession(id: Int, ownerMailbox: ActorRef[OwnerCommand]) {
  // TODO: hacerlo mas POO a esta clase y mostrar ambas formas de manejar el modelo de actores
  def start(): Behavior[RoomCommand] = Behaviors.setup { context =>
    // TODO: podría agregarse un atributo a pesar de no venir en el constructor?
    val host = context.spawn(Host(context.self), "host-" + id)
    waitingOwnerCommand(host)
  }

  def waitingOwnerCommand(host: ActorRef[Host.HostCommand]): Behavior[RoomCommand] =
    Behaviors.receive { (context, message) => {
    message match {
      case AuctionItem(item) =>
        println("Room " + id + ": Subasté item  " + item)
        // TODO: hacer N clientes random?
        val clients = spawnClients(10, List.empty, context)
        host ! AuctionItemWithThisClients(item, clients)
        Behaviors.same
      case FinishedSession() =>
        ownerMailbox ! SendNextItem(context.self)
        Behaviors.same
      case NoMoreItems() =>
        println("Room " + id + ": Finalizo ejecución")
        Behaviors.stopped
    }
  }}

  def waitingHostCommand(host: ActorRef[Host.HostCommand]): Behavior[RoomCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case
      }
    }
  }

  def spawnClients(amount: Int,
                   clients: List[ActorRef[ClientCommand]],
                   context: ActorContext[RoomCommand]): List[ActorRef[ClientCommand]] = {
    if (amount == clients.length) {
      clients
    } else {
      val client_id = clients.length + 1
      val client = context.spawn(Client(client_id), "cliente-" + client_id)
      spawnClients(amount, client :: clients, context)
    }
  }
}
