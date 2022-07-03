import Host.{AuctionItemWithThisClients, CloseAuction, HostCommand}
import Owner.{OwnerCommand, SendNextItem}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

sealed trait RoomCommand

final case class AuctionItem(item: Item) extends RoomCommand
final case class NoMoreItems() extends RoomCommand
final case class FinishedSession() extends RoomCommand

class RoomSession(val id: Int, val ownerMailbox: ActorRef[OwnerCommand], var itemsOffered: Int = 0 ) {
  var host: Option[ActorRef[HostCommand]] = None

  def start(): Behavior[RoomCommand] = Behaviors.setup { context =>
    host = Some(context.spawn(Host(id, context.self), "host-" + id))
    new WaitingForNextItemBehavior(context)
  }

  class WaitingToFinishAuction(context: ActorContext[RoomCommand]) extends AbstractBehavior[RoomCommand](context) {
    override def onMessage(message: RoomCommand): Behavior[RoomCommand] = {
      message match {
        case FinishedSession() =>
          ownerMailbox ! SendNextItem(context.self)
          new WaitingForNextItemBehavior(context)
        case _ =>
          this
      }
    }
  }

  class WaitingForNextItemBehavior(context: ActorContext[RoomCommand])
    extends AbstractBehavior[RoomCommand](context) {
    override def onMessage(message: RoomCommand): Behavior[RoomCommand] = {
      message match {
        case AuctionItem(item) =>
          val clients = spawnClients(2, List.empty, context)
          val user = context.spawn(new UserClient(clients.length + 1, id)(), f"user-$id-$itemsOffered")
          host.get ! AuctionItemWithThisClients(item, user :: clients)
          new WaitingToFinishAuction(context)
        case NoMoreItems() =>
          host.get ! CloseAuction()
          Behaviors.stopped
        case _ =>
          this
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
}
