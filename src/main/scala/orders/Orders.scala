package org.challange.bursa
package orders

import orders.Types.{Canceled, ExistingLimitOrder, ExistingOrder, Listed, NewLimitOrder, OrderEvent, OrderId, OrderStateChangeEvent}

trait Orders {
  def createLimitOrder(order: NewLimitOrder, listener: OrderEvent => Unit = _ => ()): ExistingLimitOrder
  def cancelOrder(id: OrderId): Option[ExistingOrder]
  def getOrder(id: OrderId): Option[ExistingOrder]
}

object Orders {
  def apply(): Orders = new OrdersImpl()
}

case class OrdersImpl() extends Orders {
  private val _orders = collection.mutable.HashMap[OrderId, ExistingOrder]()
  private val _listeners = collection.mutable.HashMap[OrderId, OrderEvent => Unit]()

  override def createLimitOrder(o: NewLimitOrder, listener: OrderEvent => Unit): ExistingLimitOrder = {
    val listedOrder = new ExistingLimitOrder(o.typ, o.price, o.quantity, o.assetId, Listed, System.currentTimeMillis())
    _orders.put(listedOrder.id, listedOrder)
    _listeners.put(listedOrder.id, listener)
    listedOrder
  }
  override def cancelOrder(id: OrderId): Option[ExistingOrder] = {
    val maybeExistingOrder: Option[ExistingOrder] = _orders.get(id)
    val maybeCanceledOrder: Option[ExistingOrder] = _orders.updateWith(id) {
      _.map(_.copy(state = Canceled, stateChangedAt = System.currentTimeMillis()))
    }

    (maybeExistingOrder, maybeCanceledOrder) match {
      case (Some(previous), Some(curr)) if previous.state != curr.state =>
        _listeners(id).apply(OrderStateChangeEvent(id, previous.state, curr.state, curr.stateChangedAt))
      case _ => // nothing to notify
    }

    maybeCanceledOrder
  }

  override def getOrder(id: OrderId): Option[ExistingOrder] = {
    _orders.get(id)
  }
}

