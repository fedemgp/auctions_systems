import Host.{HostCommand, ItemOffer}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import scala.util.Random

object Client {
  sealed trait ClientCommand

  final case class StartingOfferOfItemAt(item: Item, replyTo: ActorRef[HostCommand]) extends ClientCommand

  final case class ItemAt(value: Int, replyTo: ActorRef[HostCommand]) extends ClientCommand

  final case class AuctionEnded() extends ClientCommand

  def apply(clientId: Int, roomId: Int): Behavior[ClientCommand] = {
    val rnd = new Random()
    // TODO: refactor randomization
    val budget = rnd.between(10, 300)
    offerLogic(clientId, roomId, budget, rnd)
  }

  // le paso el random para no romper las bolas con la seed
  def offerLogic(clientId: Int, roomId: Int, budget: Int, rng: Random): Behavior[ClientCommand] = Behaviors.receive {
    (context, message) => {
      message match {
        case StartingOfferOfItemAt(item, host) =>
          println(f"[CLIENT $clientId, from room: $roomId] new item ${item.name} arrived, with value ${item.value}")
          // si el cliente no tiene plata suficiente o lo maximo que esta dispuesto a ofertar es menor que el precio
          // del item, entonces no oferta nada
          // 5% de chance por cada
          if (item.value > budget) {
            println(f"[CLIENT $clientId, from room: $roomId] item ${item.name} is too expensive, i won't offer this")
            Behaviors.stopped
          }

          sendRandomOffer(roomId, budget, rng, context, host)
          offerLogic(clientId, roomId, budget, rng)
        case ItemAt(value, host) =>
          println(f"[CLIENT $clientId, from room: $roomId] new item value is $value")
          // 5% chance of randomly giving up
          val giveUp = rng.nextFloat() > 0.95
          if (value > budget || giveUp) {
            println(f"[CLIENT $clientId, from room: $roomId] i won't continue offering this")
            Behaviors.stopped
          }
          sendRandomOffer(budget, value, rng, context, host)
          offerLogic(clientId, roomId, budget, rng)
        case AuctionEnded() =>
          println(f"[CLIENT $clientId, from room: $roomId] auction ended")
          Behaviors.same
      }
    }
  }

  // TODO: aca si vendria bien POO...
  private def sendRandomOffer(budget: Int,
                              value: Int,
                              rng: Random,
                              context: ActorContext[ClientCommand],
                              host: ActorRef[HostCommand]): Unit = {
    val offerOpt = randomOffering(budget, value, rng)
    offerOpt match {
      case Some(offer) =>
        host ! ItemOffer(offer, context.self)
    }
  }

  // TODO: corregir esta cosa horrible
  private def randomOffering(budget: Int, value: Int, rng: Random): Option[Int] = {
    val willIOffer = rng.nextFloat() > 0.5
    if (willIOffer) {
      val offer = math.min(budget, rng.between(value + 1, value + 20))
      return Some(offer)
    }
    None
  }

}
