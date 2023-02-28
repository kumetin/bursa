package org.challange.bursa

import assets.Assets

import org.challange.bursa.assets.Types.{Asset, AssetId}
import org.challange.bursa.orders.{Orders, Types}
import org.challange.bursa.orders.Types.{Buy, ExistingLimitOrder, ExistingOrder, NewLimitOrder, OrderEvent, OrderId}

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
    def loggingEventListener(event: OrderEvent): Unit = {
      event match {
        case Types.OrderStateChangeEvent(id, oldState, newState, at) =>
          println(s"\tEventListener: Order '$id' status changed from '$oldState' to '$newState' at $at")
      }
    }

    // Here we create a new limit order, subscribing our listener for events on that order
    val existingGoogleBuyLimitOrder: ExistingLimitOrder =
      sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Buy, 50, 100), loggingEventListener)

    val canceledOrder: ExistingOrder =
      cancelOrder(ordersMgr, existingGoogleBuyLimitOrder.id).getOrElse(throw new RuntimeException("BUG BUG BUG"))

    val maybeOrder: Option[ExistingOrder] =
      getOrder(ordersMgr, canceledOrder.id)
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
                             listener: OrderEvent => Unit = _ => ()): ExistingLimitOrder = {
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
}