import Host.{AuctionItemWithThisClients, CloseAuction, HostCommand}
import Owner.{OwnerCommand, SendNextItem}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

sealed trait RoomCommand

final case class AuctionItem(item: Item) extends RoomCommand
final case class NoMoreItems() extends RoomCommand
final case class FinishedSession() extends RoomCommand

class RoomSession(val id: Int, val ownerMailbox: ActorRef[OwnerCommand], var itemsOffered: Int = 0 ) {
  def start(): Behavior[RoomCommand] = Behaviors.setup { context =>
    val host = context.spawn(Host(id, context.self), "host-" + id)
    waitingOwnerCommand(host)
  }

  def waitingOwnerCommand(host: ActorRef[Host.HostCommand]): Behavior[RoomCommand] =
    Behaviors.receive { (context, message) => {
      message match {
        case AuctionItem(item) =>
          // TODO: hacer N clientes random?
          val clients = spawnClients(2, List.empty, context)
          val user = context.spawn(new UserClient(clients.length + 1, id)(), f"user-$id-$itemsOffered")
          host ! AuctionItemWithThisClients(item, user :: clients)
          Behaviors.same
        case FinishedSession() =>
          ownerMailbox ! SendNextItem(context.self)
          Behaviors.same
        case NoMoreItems() =>
          host ! CloseAuction()
          Behaviors.stopped
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
      val client = context.spawn(new BotClient(client_id, id)(), f"cliente-$client_id-$id-$itemsOffered")
      itemsOffered += 1
      spawnClients(amount, client :: clients, context)
    }
  }
}
