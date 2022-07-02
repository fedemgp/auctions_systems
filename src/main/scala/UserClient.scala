import Host.{HostCommand, ItemOffer}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.io.StdIn

object UserClient extends AbstractClient {

  override def apply(clientId: Int, roomId: Int): Behavior[ClientCommand] = {
    this.clientId = clientId
    this.roomId = roomId
    run()
  }

  def run(): Behavior[ClientCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case StartingOfferOfItemAt(item, host) =>
          println(f"[User] Starting offer with initial value ${item.value}.")
          readCommand(context.self, host)
        case ItemAt(value, host) =>
          readCommand(context.self, host)
        case AuctionEnded() =>
          println("[User] Auction ended")
          Behaviors.stopped
        case _ =>
          Behaviors.ignore
      }
    }
  }

  def readCommand(self: ActorRef[ClientCommand], host: ActorRef[HostCommand]): Behavior[ClientCommand] = {
    var command = ""
    do {
      print("Please, enter new offer or 'surrender' to exit the auction: ")
      command = StdIn.readLine()
    } while(!isValid(command))

    if (command == "surrender") {
      println("Surrendering")
      surrender()
    } else {
      host ! ItemOffer(command.toInt, self)
      run()
    }
  }

  def isValid(command: String): Boolean = {
    val res = (command == "surrender") || (command.forall(Character.isDigit))
    res
  }
}
