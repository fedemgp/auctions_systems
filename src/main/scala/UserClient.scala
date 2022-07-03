import Host.{HostCommand, ItemOffer}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Success

class UserClient(clientId: Int, roomId: Int) extends AbstractClient(clientId, roomId) {

  override def apply(): Behavior[ClientCommand] = {
    run(None)
  }

  def run(readFuture: Option[Future[String]]): Behavior[ClientCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case StartingOfferOfItemAt(item, host) =>
          val newReadFuture = readCommand(context.self, host)
          run(newReadFuture)
        case ItemAt(_, host) =>
          readFuture match {
            case Some(future) =>
              if (future.isCompleted) {
                val newReadFuture = readCommand(context.self, host)
                run(newReadFuture)
              } else {
                Behaviors.same
              }
            case None =>
              val newReadFuture = readCommand(context.self, host)
              run(newReadFuture)
          }
        case AuctionEnded() =>
          println("[User] Auction ended")
          Behaviors.stopped
        case _ =>
          Behaviors.ignore
      }
    }
  }

  def readCommand(self: ActorRef[ClientCommand], host: ActorRef[HostCommand]): Option[Future[String]] = {
    val readFuture = Future({
      var command = ""
      do {
        println(Console.RED + "Please, enter new offer or 'surrender' to exit the auction: " + Console.RESET)
        command = StdIn.readLine()
      } while (!isValid(command))
      command
    })
    readFuture.onComplete({
      case Success(command) =>
        if (command == "surrender") {
          println("Surrendering")
          surrender()
        } else {
          host ! ItemOffer(command.toInt, self)
        }
      case _ =>
    })
    Some(readFuture)
  }

  // ojo, si command == "" => command.forall(Character.isDigit) == True
  def isValid(command: String): Boolean = {
    val res = command.nonEmpty && ((command == "surrender") || command.forall(Character.isDigit))
    res
  }
}
