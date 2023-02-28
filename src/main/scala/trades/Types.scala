package org.challange.bursa
package trades

import events.Types.BursaEvent

import org.challange.bursa.assets.Types.AssetId

object Types {
  case class TradeEvent(assetId: AssetId, quantity: Int, price: Price, ts: Long) extends BursaEvent
}
