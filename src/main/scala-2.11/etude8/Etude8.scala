package etude8

import java.util.Scanner

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scalax.file.Path

/**
 * Created by tinhtooaung on 5/05/15.
 */

object Etude8 {

  def main(args: Array[String]): Unit = {
    if(args.isEmpty){
      println("Usage: [startWord] [endWord] {length}")
      return
    }
    val path: Path = Path.fromString("/tmp/temp-neo-test")
    Try(path.deleteRecursively(continueOnFailure = false))

    var inputWords = getInputData
    val startWord = args(0)
    val endWord = args(1)
    inputWords =  endWord :: startWord :: inputWords
    val dictionary = new Dictionary(inputWords, startWord, endWord)

    if(args.length == 2){
      dictionary.getShortestPath
    }else{
      dictionary.getPath(args(2).toInt)
    }
  }

  def getInputData : List[String]= {
    val inputs = ListBuffer.empty[String]
    val scanner = new Scanner(System.in)
    while(scanner.hasNextLine){
      inputs += scanner.next()
    }
    inputs.toList
  }
}
