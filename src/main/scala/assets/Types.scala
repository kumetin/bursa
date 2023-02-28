package org.challange.bursa
package assets

object Types {
  type AssetId = String

  case class Asset(id: AssetId, displayName: String)
}
