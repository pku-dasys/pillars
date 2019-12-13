package tetriski.pillars

import java.io.{File, PrintWriter}

import scala.collection.mutable.{ArrayBuffer, Queue}

//This class is obtained from GenerateConnection and records the connection information of the two blocks mentioned above.
//The outArray and inArray individually contains the ports which have the intrinsic mapping sequences.
class Connect(connectArray: ArrayBuffer[List[List[String]]]) {

  val outArray = ArrayBuffer[List[String]]()
  val inArray = ArrayBuffer[List[String]]()
  connectArray.foreach(t => outArray.append(t(0)))
  connectArray.foreach(t => inArray.append(t(1)))

  var mapRelation = Map[List[String], ArrayBuffer[List[String]]]()


  for (i <- 0 until outArray.length) {
    //Connect the corresponding ports via mapping
    if (mapRelation.contains(outArray(i))) {
      mapRelation(outArray(i)).append(inArray(i))
    } else {
      mapRelation += (outArray(i) -> ArrayBuffer(inArray(i)))
    }

  }
  mapRelation = simplify()

  // (to be defined)
  //"cgra/b_0/b_0_0:out" should be equal to "cgra/b_0/b_0_0:out/adder0:out"
  def simplify(): Map[List[String], ArrayBuffer[List[String]]] = {
    var ret = Map[List[String], ArrayBuffer[List[String]]]()
    val srcs = outArray.toSet
    val dsts = inArray.toSet
    val sources = srcs&~(srcs.&(dsts))
    val sinks = dsts&~(srcs.&(dsts))
    //    println(srcs)
    //    println(dsts)
    //    println(sources)
    //    println(sources.contains(List("cgra/","input_1")))
    //    println(sources.contains(List("cgra/","input_0")))
    for (src <- sources){
      //BFS
      var targets = ArrayBuffer[List[String]]()
      var queue = Queue[List[String]]()
      mapRelation(src).map(i => queue.enqueue(i))
      while (!queue.isEmpty){
        val temp = queue.dequeue()
        if(sinks(temp)){
          targets.append(temp)
        }else{
          mapRelation(temp).map(i => queue.enqueue(i))
        }
      }
      ret += (src -> targets)
    }
    //    println(ret(List("cgra/","input_0")))
    //    println(ret(List("cgra/","input_1")))
    ret
  }

  //Get connect relations
  def getConnect(): Map[List[String], ArrayBuffer[List[String]]] = {
    mapRelation
  }

  //Save connection information as connect.json
  def dumpConnect() = {
    val writer = new PrintWriter(new File("connect.json"))

    writer.flush()

    def printConnect(src: List[String], dsts: ArrayBuffer[List[String]], comma: Boolean): Unit = {
      def printPort(port: List[String]): Unit = {
        var ii = 0
        for (i <- 0 until port.size) {
          var str = port(i)
          var label = ""
          if (ii % 2 != 0) label = "/"
          else label = ":"
          if (str(str.size - 1) == '/') writer.print(str)
          else {
            if (i != port.size - 1) writer.print(str + label)
            else writer.print(str)
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
        if (i != dsts.size - 1) writer.print("\",\n")
        else writer.print("\"")
      }
      if (comma) writer.print("],\n")
      else writer.print("]\n")
    }

    writer.print("{")
    mapRelation.foreach((x) => printConnect(x._1, x._2, !x.equals(mapRelation.last)))
    writer.print("}")
    writer.close()
  }


}