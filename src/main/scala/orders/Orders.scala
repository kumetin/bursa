package org.challange.bursa
package orders

import orders.Types.{ExistingOrder, Canceled, ExistingLimitOrder, Listed, NewLimitOrder, OrderId}

trait Orders {
  def createLimitOrder(order: NewLimitOrder): ExistingLimitOrder
  def cancelOrder(id: OrderId): Option[ExistingOrder]
  def getOrder(id: OrderId): Option[ExistingOrder]
}

object Orders {
  def apply(): Orders = new OrdersImpl()
}

case class OrdersImpl() extends Orders {
  private val _orders = collection.mutable.HashMap[OrderId, ExistingOrder]()

  override def createLimitOrder(o: NewLimitOrder): ExistingLimitOrder = {
    val listedOrder = new ExistingLimitOrder(o.typ, o.price, o.quantity, o.assetId, Listed, System.currentTimeMillis())
    _orders.put(listedOrder.id, listedOrder)
    listedOrder
  }
  override def cancelOrder(id: OrderId): Option[ExistingOrder] = {
    val maybeOrder: Option[ExistingOrder] = _orders.get(id)
    val maybeCanceledOrder = _orders.updateWith(id) {
      _.map(_.copy(state = Canceled, stateChangedAt = System.currentTimeMillis()))
    }
    maybeCanceledOrder
  }

  override def getOrder(id: OrderId): Option[ExistingOrder] = {
    _orders.get(id)
  }
}

