package org.challange.bursa

import assets.Assets
import assets.Types.{Asset, AssetId}
import events.Types.BursaEvent
import orders.Orders
import orders.Types._
import trades.Types.TradeEvent

object Main {

  def main(args: Array[String]): Unit = {

    println("> Hello Bursa!")

    val assetsMgr = Assets()
    showAssets(assetsMgr.list())
    addAsset(assetsMgr, Asset("AAPL", "Apple Inc."))
    showAssets(assetsMgr.list())
    removeAsset(assetsMgr, "AAPL")
    showAssets(assetsMgr.list())
    addAsset(assetsMgr, Asset("GOOG", "Google Inc."))
    showAssets(assetsMgr.list())

    val ordersMgr = Orders()

    // Here we create a listener which simply logs the events it intercepts
    // This covers 'Support receiving updates on the state of an order through push notifications.'
    def orderEventListener(event: BursaEvent): Unit = {
      event match {
        case OrderStateChangeEvent(id, oldState, newState, ts) =>
          println(s"\tEventListener: Order '$id' status changed from '$oldState' to '$newState' at $ts")
      }
    }

    // Here we create a listener which simply logs the events it intercepts
    // This covers 'Support receiving updates on executed trades, including the asset, quantity, price, and time of
    // the trade.'
    def tradeEventListener(event: BursaEvent): Unit = {
      event match {
        case TradeEvent(assetId, quantity, price, ts) =>
          println(s"\tEventListener: Asset '$assetId' traded ; $quantity units for $price per unit, at $ts")
      }
    }

    // Here we create a new limit order, subscribing our listener for events on that order
    val existingGoogleBuyLimitOrder: ExistingLimitOrder =
      sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Buy, 100, 50), orderEventListener)

    ordersMgr.registerListener(tradeEventListener)

    val canceledOrder: ExistingOrder =
      cancelOrder(ordersMgr, existingGoogleBuyLimitOrder.id).getOrElse(throw new RuntimeException("BUG BUG BUG"))

    val maybeOrder: Option[ExistingOrder] =
      getOrder(ordersMgr, canceledOrder.id)

    sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Buy, 95, 100), orderEventListener)
    sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Sell, 105, 200), orderEventListener)
    sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Sell, 110, 50), orderEventListener)
    showSupplyDemand(ordersMgr, "GOOG")
    sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Buy, 98, 75), orderEventListener)
    sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Sell, 97, 100), orderEventListener)
    showSupplyDemand(ordersMgr, "GOOG")

  }

  private def showAssets(assets: Seq[Asset]): Unit = {
    print(s"> There are currently ${assets.size} assets")
    if (assets.nonEmpty) {
      println(s": ${assets.mkString("\n\t", "\n\t", "")}")
    } else
      println(".")
  }

  private def addAsset(assets: Assets, asset: Asset): Unit = {
    assets.add(asset)
    println(s"> $asset added.")
  }

  private def removeAsset(assets: Assets, assetId: AssetId): Unit = {
    assets.remove(assetId)
    println(s"> Asset id '${assetId}' removed.")
  }

  private def sendLimitOrder(orders: Orders, newOrder: NewLimitOrder,
                             listener: BursaEvent => Unit = _ => ()): ExistingLimitOrder = {
    println(s"> Limit Order : Creating order $newOrder")
    val order: ExistingLimitOrder = orders.createLimitOrder(newOrder, listener)
    println(s"> Limit Order : '${order.id}' ${order.stateString}")
    order
  }

  private def cancelOrder(ordersMgr: Orders, id: OrderId): Option[ExistingOrder] = {
    println(s"> Cancel Order '$id")
    val result = ordersMgr.cancelOrder(id)
    result match {
      case Some(order) => println(s"> Cancel Order '$id': ${order.stateString}")
      case None => println(s"> Cancel Order '$id': order id not found")
    }
    result
  }

  def getOrder(ordersMgr: Orders, id: OrderId): Option[ExistingOrder] = {
    val result = ordersMgr.getOrder(id)
    result match {
      case Some(order) => println(s"> Query Order '$id': ${order.stateString}")
      case None => println(s"> Query Order '$id': order id not found")
    }
    result
  }

  def showSupplyDemand(ordersMgr: Orders, assetId: AssetId): Unit = {
    val supplyDemand = ordersMgr.getAssetSupplyDemand(assetId)
    println(s"> Show Supply & Demand for '$assetId'")
    supplyDemand.demandByPrice.foreach{ case (price, quantity) =>
      println(s"\tThere are $quantity shares available for purchase at $price per share")
    }
    supplyDemand.supplyByPrice.foreach { case (price, quantity) =>
      println(s"\tThere are $quantity shares available for sell at $price per share")
    }
  }

}