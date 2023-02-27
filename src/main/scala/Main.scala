package org.challange.bursa

import assets.Assets

import org.challange.bursa.assets.Types.Asset

object Main {
  def main(args: Array[String]): Unit = {
    println("> Hello Bursa!")
    val assets = Assets()
    println(s"> There are currently ${assets.list().size} assets")
    assets.add(Asset("AAPL", "Apple Inc."))
    println(s"> Asset AAPL added. This is the updated asset list:\n${assets.list.mkString("\t", "\n", "")}")
    assets.remove("AAPL")
    println(s"> Asset AAPL removed. There are currently ${assets.list().size} assets")
  }
}