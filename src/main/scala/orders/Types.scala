package org.challange.bursa
package orders

import assets.Types.AssetId

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

  case class NewLimitOrder(assetId: AssetId, typ: OrderType, price: Float, quantity: Int)

  class ExistingLimitOrder(typ: OrderType,
                           price: Float,
                           quantity: Int,
                           override val assetId: OrderId,
                           override val state: OrderState,
                           override val stateChangedAt: Long) extends ExistingOrder(
    s"$assetId-$stateChangedAt-Limit-$typ-$price-$quantity", assetId, state, stateChangedAt) {
  }

  sealed trait OrderType
  case object Buy extends OrderType
  case object Sell extends OrderType

  sealed trait OrderState
  case object Listed extends OrderState
  case object Canceled extends OrderState
  case object Executed extends OrderState

  sealed trait OrderEvent
  case class OrderStateChangeEvent(id: OrderId, oldState: OrderState, newState: OrderState, at: Long) extends OrderEvent
}
