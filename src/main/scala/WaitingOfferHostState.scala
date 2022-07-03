class WaitingOfferHostState(currentValue: Int, oldValue: Int) extends HostState {

  override def execute(): Unit = {
    println(f"[Host] Repeated offer with value $currentValue (old value $oldValue)")
  }
}
