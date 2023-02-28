package org.challange.bursa

import assets.Assets

import org.challange.bursa.assets.Types.{Asset, AssetId}
import org.challange.bursa.orders.Orders
import org.challange.bursa.orders.Types.{ExistingOrder, Buy, ExistingLimitOrder, NewLimitOrder, OrderId}

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
    val existingGoogleBuyLimitOrder: ExistingLimitOrder =
      sendLimitOrder(ordersMgr, NewLimitOrder("GOOG", Buy, 50, 100))
    val canceledOrder: ExistingOrder =
      cancelOrder(ordersMgr, existingGoogleBuyLimitOrder.id).getOrElse(throw new RuntimeException("BUG BUG BUG"))
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

  private def sendLimitOrder(orders: Orders, newOrder: NewLimitOrder): ExistingLimitOrder = {
    val order: ExistingLimitOrder = orders.createLimitOrder(newOrder)
    println(s"> Limit Order : '${order.id}' ${order.stateString}")
    order
  }

  private def cancelOrder(ordersMgr: Orders, id: OrderId): Option[ExistingOrder] = {
    print(s"> Cancel Order : '$id' ")
    val result = ordersMgr.cancelOrder(id)
    result match {
      case Some(order) => println(s"${order.stateString}")
      case None => println("Not found")
    }
    result
  }

  def getOrder(ordersMgr: Orders, id: OrderId): Option[ExistingOrder] = {
    print(s"> Query Order '${id}': ")
    val result = ordersMgr.getOrder(id)
    result match {
      case Some(order) => println(order)
      case None => println("Not found")
    }
    result
  }
}