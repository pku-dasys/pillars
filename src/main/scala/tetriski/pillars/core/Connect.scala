package tetriski.pillars.core

import java.io.{File, PrintWriter}

import scala.collection.mutable.{ArrayBuffer, Queue}

/** A class containing the connections between ports.
 *
 * @param connectArray Array(List(src, dst)),
 *                     where src and dst are lists representing ports which have a connection
 */
class Connect(connectArray: ArrayBuffer[List[List[String]]]) {
  /** An array of sink ports.
   */
  val outArray = ArrayBuffer[List[String]]()

  /** An array of source ports.
   */
  val inArray = ArrayBuffer[List[String]]()

  //Initialize outArray and inArray.
  connectArray.foreach(t => outArray.append(t(0)))
  connectArray.foreach(t => inArray.append(t(1)))

  /** A map showing the connections between ports in String format.
   */
  var mapRelation = Map[List[String], ArrayBuffer[List[String]]]()


  for (i <- 0 until outArray.length) {
    //Connect the corresponding ports via mapping
    if (mapRelation.contains(outArray(i))) {
      mapRelation(outArray(i)).append(inArray(i))
    } else {
      mapRelation += (outArray(i) -> ArrayBuffer(inArray(i)))
    }

  }
  mapRelation = flatten()

  /** Flatten the architecture.
   *
   * @return a flattened map showing the connections between ports in String format.
   */
  def flatten(): Map[List[String], ArrayBuffer[List[String]]] = {
    var ret = Map[List[String], ArrayBuffer[List[String]]]()
    val srcs = outArray.toSet
    val dsts = inArray.toSet
    val sources = srcs &~ srcs.&(dsts)
    val sinks = dsts &~ srcs.&(dsts)
    for (src <- sources) {
      //BFS
      val targets = ArrayBuffer[List[String]]()
      val queue = Queue[List[String]]()
      mapRelation(src).map(i => queue.enqueue(i))
      while (!queue.isEmpty) {
        val temp = queue.dequeue()
        if (sinks(temp)) {
          targets.append(temp)
        } else {
          mapRelation(temp).map(i => queue.enqueue(i))
        }
      }
      ret += (src -> targets)
    }
    ret
  }

  /** Get the map showing the connections between ports in String format.
   *
   * @return the map showing the connections between ports in String format.
   */
  def getConnect(): Map[List[String], ArrayBuffer[List[String]]] = {
    mapRelation
  }

  /** Save connection information as connect.json.
   */
  def dumpConnect(): Unit = {
    val writer = new PrintWriter(new File("connect.json"))

    writer.flush()

    /** Print the connection between a source port and its sink ports.
     *
     * @param src   a list representing the source port
     * @param dsts  lists representing the sink ports
     * @param comma use comma or not
     */
    def printConnect(src: List[String], dsts: ArrayBuffer[List[String]], comma: Boolean): Unit = {
      /** Print the information of a port.
       *
       * @param port a list representing this port
       */
      def printPort(port: List[String]): Unit = {
        var ii = 0
        for (i <- 0 until port.size) {
          val str = port(i)
          var label = ""
          if (ii % 2 != 0) {
            label = "/"
          }
          else {
            label = ":"
          }
          if (str(str.size - 1) == '/') {
            writer.print(str)
          }
          else {
            if (i != port.size - 1) {
              writer.print(str + label)
            }
            else {
              writer.print(str)
            }
            ii += 1
          }
        }
      }

      writer.print("\"")
      printPort(src)
      writer.print("\":\n[")
      for (i <- 0 until dsts.size) {
        writer.print("\"")
        printPort(dsts(i))
        if (i != dsts.size - 1) {
          writer.print("\",\n")
        }
        else {
          writer.print("\"")
        }
      }
      if (comma) {
        writer.print("],\n")
      }
      else {
        writer.print("]\n")
      }
    }

    writer.print("{")
    mapRelation.foreach((x) => printConnect(x._1, x._2, !x.equals(mapRelation.last)))
    writer.print("}")
    writer.close()
  }

}