package org.challange.bursa
package orders

import assets.Types.AssetId
import events.Types.BursaEvent

object Types {
  type OrderId = String

  case class ExistingOrder (
    id: OrderId,
    assetId: AssetId,
    state: OrderState,
    stateChangedAt: Long
  ) {
    final def stateString: String = s"${state} at ${stateChangedAt}"
  }

  case class NewLimitOrder(assetId: AssetId, typ: OrderType, price: Price, quantity: Int)

  class ExistingLimitOrder(val typ: OrderType,
                           val price: Price,
                           private var _quantity: Int,
                           override val assetId: OrderId,
                           override val state: OrderState,
                           override val stateChangedAt: Long) extends ExistingOrder (
    s"$assetId-$stateChangedAt-Limit-$typ-$price-${_quantity}", assetId, state, stateChangedAt) {
    def getQuantity(): Int = _quantity
    def increaseQuantityBy(more: Int) { _quantity += more }
    def decreaseQuantityBy(less: Int) { _quantity -= less }
  }

  sealed trait OrderType
  case object Buy extends OrderType
  case object Sell extends OrderType

  sealed trait OrderState
  case object Listed extends OrderState
  case object Canceled extends OrderState
  case object Executed extends OrderState

  sealed trait OrderEvent extends BursaEvent
  case class OrderStateChangeEvent(id: OrderId,
                                   oldState: OrderState,
                                   newState: OrderState,
                                   ts: Long) extends OrderEvent
  case class AssetSupplyDemand(
    assetId: AssetId,
    supplyByPrice: Map[Price, Int],
    demandByPrice: Map[Price, Int]
  )
}
