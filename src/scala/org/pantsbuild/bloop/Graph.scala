package org.pantsbuild.bloop

import scala.collection.mutable
import ujson._
import bloop.config.Config.Project

case class Graph(
    export: Value,
    index: String => Int,
    rindex: Int => String,
    graph: Array[Array[Int]]
)

object Graph {
  def fromJson(export: Value): Graph = {
    val targets = export.obj("targets").obj
    val index = targets.keysIterator.zipWithIndex.toMap
    val rindex = index.iterator.map(_.swap).toMap
    val edges = new Array[Array[Int]](index.size)
    index.foreach {
      case (key, i) =>
        val ts = targets(key).obj("targets").arr
        val deps = ts.iterator.map(dep => index(dep.str)).toArray
        edges(i) = deps
    }
    Graph(export, index.apply _, rindex.apply _, edges)
  }
}
