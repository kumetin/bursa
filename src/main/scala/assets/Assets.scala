package org.challange.bursa
package assets

import assets.Types.Asset

import scala.collection.mutable.ListBuffer

trait Assets {
  def add(asset: Asset): Unit
  def remove(id: String): Unit
  def list(): Seq[Asset]
}

object Assets {
  def apply(): Assets = AssetsImpl()
}

private [assets] case class AssetsImpl() extends Assets {

  private val assetsList = collection.mutable.ListBuffer[Asset]()

  override def add(asset: Asset): Unit = {
    assetsList += asset
  }
  override def remove(id: String): Unit = {
    // TODO: switch to hashmap in order to get OTC of O(1)
    assetsList.find(_.id == id).foreach(assetsList -= _)
  }
  override def list(): Seq[Asset] = {
    assetsList.toSeq
  }
}
