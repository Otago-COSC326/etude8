/**
 * Created by tinhtooaung on 5/05/15.
 */

package etude8

import eu.fakod.neo4jscala.{Cypher, EmbeddedGraphDatabaseServiceProvider, Neo4jWrapper}
import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphdb._
import org.neo4j.kernel.Traversal

import scala.collection.JavaConversions._

class Dictionary(words : List[String],
                 val startWord: String,
                 val endWord: String) extends Neo4jWrapper with EmbeddedGraphDatabaseServiceProvider
with Cypher{

  def neo4jStoreDir = "/tmp/temp-neo-test"

  constructGraph()

  def getChainableWords(sourceWord: String) : List[String] = {
    words.filter { word =>
      var noMatchesCount = 0
      for (c <- word.zipWithIndex){
        if(sourceWord(c._2) != c._1){
          noMatchesCount += 1
        }
      }
      noMatchesCount == 1
    }
  }

  def getShortestPath = {
    try{
      val start = getNodeByValue(value = startWord)
      val end = getNodeByValue(value = endWord)

      val finder = GraphAlgoFactory.shortestPath(
        Traversal.expanderForTypes("chainable", Direction.OUTGOING), 30
      )
      val resultList = for{
        node <- finder.findAllPaths(start, end).head.nodes()
      }yield node("value").getOrElse("")
      println(resultList.mkString(" "))
    }catch {
      case _:Exception => ""
    }
  }

  def getPath(length: Int) = {
    try{
      val start = getNodeByValue(value = startWord)
      val end = getNodeByValue(value = endWord)

      val finder = GraphAlgoFactory.pathsWithLength(
        Traversal.pathExpanderForTypes("chainable", Direction.OUTGOING), length - 1
      )
      val resultList = for{
        node <- finder.findAllPaths(start, end).head.nodes()
      }yield node("value").getOrElse("")
      println(resultList.mkString(" "))
    }catch {
      case _:Exception => ""
    }
  }

  private def constructGraph()= {
    withTx {
      implicit neo =>
        val wordIndex = indexManager.forNodes("words")
        for {
          word <- words
          chainableWords = getChainableWords(word)
          chainableWord <- chainableWords if chainableWords.nonEmpty
        }{
          var start = getNodeByValue(value = word)
          if(start == null){
            start = createNode
            start("value") = word
            wordIndex.add(start, "value", word)
          }

          var end = getNodeByValue(value = chainableWord)
          if(end == null && chainableWord != null){
            end = createNode
            end("value") = chainableWord
            wordIndex.add(end, "value", chainableWord)
          }
          start --> "chainable" --> end
        }
      //        println("Node count: " + getAllNodes.size)
    }

  }

  def getNodeByValue(index: String = "words", value: String) = {
    val wordIndex  = indexManager(ds).forNodes("words")
    val startNode = wordIndex.get("value", value).getSingle
    startNode
  }
}
