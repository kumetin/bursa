package org.challange.bursa
package orders

import assets.Types.AssetId
import events.Types.BursaEvent
import orders.Types._
import trades.Types.TradeEvent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Orders {
  /**
   * This methods does two things:
   * 1. Places a limit order in the order book
   * 2. Looks for a matching order in the order book, if found, executes both orders
   */
  def createLimitOrder(order: NewLimitOrder, listener: BursaEvent => Unit = _ => ()): ExistingLimitOrder

  def cancelOrder(id: OrderId): Option[ExistingOrder]

  def getOrder(id: OrderId): Option[ExistingOrder]

  def registerListener(handle: BursaEvent => Unit): Unit

  def getAssetSupplyDemand(id: AssetId): AssetSupplyDemand
}

object Orders {
  def apply(): Orders = new OrdersImpl()
}

// NOT thread safe. Only one thread should invoke the methods here in any given time.
case class OrdersImpl() extends Orders {
  private val _assetBuyLimitQueues = mutable.HashMap[AssetId, ListBuffer[ExistingLimitOrder]]()
  private val _assetSellLimitQueues = mutable.HashMap[AssetId, ListBuffer[ExistingLimitOrder]]()
  private val _ordersMap = mutable.HashMap[OrderId, ExistingOrder]()
  private val _ordersListeners = mutable.HashMap[OrderId, BursaEvent => Unit]()
  private val _listeners = mutable.ListBuffer[BursaEvent => Unit]()

  private def assetBuyLimitQueue(assetId: AssetId): ListBuffer[ExistingLimitOrder] = {
    _assetBuyLimitQueues.getOrElseUpdate(assetId, new ListBuffer[ExistingLimitOrder])
  }

  private def assetSellLimitQueue(assetId: AssetId): ListBuffer[ExistingLimitOrder] = {
    _assetSellLimitQueues.getOrElseUpdate(assetId, new ListBuffer[ExistingLimitOrder])
  }

  /**
   * Find the queue matching the order type (buy/sell) and asset provided in the order.
   * A BuyLimit order is enqueued into the BuyLimitQueue
   * A SellLimit order is enqueued into the SellLimitQueue
   */
  private def queue(order: ExistingLimitOrder): ListBuffer[ExistingLimitOrder] = order.typ match {
    case Types.Buy => assetBuyLimitQueue(order.assetId)
    case Types.Sell => assetSellLimitQueue(order.assetId)
  }

  /**
   * Find the opposite queue where trading opportunities for given limit order may be found
   */
  private def oppositeQueue(order: ExistingLimitOrder): ListBuffer[ExistingLimitOrder] = order.typ match {
    case Types.Buy => assetSellLimitQueue(order.assetId)
    case Types.Sell => assetBuyLimitQueue(order.assetId)
  }

  /**
   * Given two limit orders, identified which is buy and which is sell
   */
  private def sellerBuyer(o1: ExistingLimitOrder, o2: ExistingLimitOrder): (ExistingLimitOrder /*Seller*/, ExistingLimitOrder /*Buyer*/) = {
    val seller = if (o1.typ == Sell) o1 else if (o2.typ == Sell) o2 else throw new RuntimeException("seller not provided")
    val buyer = if (o1.typ == Buy) o1 else if (o2.typ == Buy) o2 else throw new RuntimeException("buyer not provided")
    (seller,buyer)
  }

  private def findAndExecuteMatchingOrders(newLimitOrder: ExistingLimitOrder): Unit = {
    def isMatch(sellOrder: ExistingLimitOrder, buyOrder: ExistingLimitOrder): Boolean = {
        sellOrder.state == Listed && buyOrder.state == Listed &&
        sellOrder.price <= buyOrder.price
    }
    val itr = oppositeQueue(newLimitOrder).iterator
    var quantityLeft = newLimitOrder.getQuantity()
    while (quantityLeft > 0 && itr.hasNext) {
      val oppositeOrder = itr.next()
      val (sellOrder, buyOrder) = sellerBuyer(newLimitOrder, oppositeOrder)
      if (isMatch(sellOrder, buyOrder)) {
        val quantityTransferred = Math.min(sellOrder.getQuantity(), buyOrder.getQuantity())
        sellOrder.decreaseQuantityBy(quantityTransferred)
        buyOrder.decreaseQuantityBy(quantityTransferred)
        List(sellOrder, buyOrder).filter(_.getQuantity() == 0).foreach(o => executeOrder(o.id))
        _listeners.foreach(_.apply(TradeEvent(newLimitOrder.assetId, quantityTransferred, sellOrder.price, System.currentTimeMillis())))
        quantityLeft -= quantityTransferred
      }
    }
  }

  private def executeOrder(orderId: OrderId): Option[ExistingOrder] = {
    val maybeExistingOrder: Option[ExistingOrder] = _ordersMap.get(orderId)
    val maybeExecutedOrder: Option[ExistingOrder] = _ordersMap.updateWith(orderId) {
      _.map(_.copy(state = Executed, stateChangedAt = System.currentTimeMillis()))
    }
    (maybeExistingOrder, maybeExecutedOrder) match {
      case (Some(previous), Some(curr)) if previous.state != curr.state =>
        _ordersListeners.get(orderId).foreach(_.apply(OrderStateChangeEvent(orderId, previous.state, curr.state, curr.stateChangedAt)))
      case _ => // nothing to notify
    }
    // Order have reached a final state, we can remove the listener
    _ordersListeners.remove(orderId)
    maybeExecutedOrder
  }


  // API Starts Here

  override def registerListener(listener: BursaEvent => Unit): Unit = {
    _listeners.addOne(listener)
  }

  override def createLimitOrder(o: NewLimitOrder, listener: BursaEvent => Unit): ExistingLimitOrder = {
    val listedOrder = new ExistingLimitOrder(o.typ, o.price, o.quantity, o.assetId, Listed, System.currentTimeMillis())
    queue(listedOrder).addOne(listedOrder)
    _ordersMap.put(listedOrder.id, listedOrder)
    _ordersListeners.put(listedOrder.id, listener)
    findAndExecuteMatchingOrders(listedOrder)
    listedOrder
  }

  override def cancelOrder(orderId: OrderId): Option[ExistingOrder] = {
    val maybeExistingOrder: Option[ExistingOrder] = _ordersMap.get(orderId)
    val maybeCanceledOrder: Option[ExistingOrder] = _ordersMap.updateWith(orderId) {
      _.map(_.copy(state = Canceled, stateChangedAt = System.currentTimeMillis()))
    }
    (maybeExistingOrder, maybeCanceledOrder) match {
      case (Some(previous), Some(curr)) if previous.state != curr.state =>
        _ordersListeners(orderId).apply(OrderStateChangeEvent(orderId, previous.state, curr.state, curr.stateChangedAt))
      case _ => // nothing to notify
    }
    // Order have reached a final state, we can remove the listener
    _ordersListeners.remove(orderId)
    maybeCanceledOrder
  }

  override def getOrder(id: OrderId): Option[ExistingOrder] = {
    _ordersMap.get(id)
  }

  override def getAssetSupplyDemand(id: AssetId): AssetSupplyDemand = {
    val supplyPerPrice =
      assetSellLimitQueue(id).filter(o => o.state == Listed && o.getQuantity() > 0)
        .groupBy(_.price).view.mapValues(_.foldLeft(0)((acc,o) => acc + o.getQuantity())).toMap
    val demandPerPrice = assetBuyLimitQueue(id).filter(o => o.state == Listed && o.getQuantity() > 0)
      .groupBy(_.price).view.mapValues(_.foldLeft(0)((acc,o) => acc + o.getQuantity())).toMap
    AssetSupplyDemand(id, supplyPerPrice, demandPerPrice)
  }
}

